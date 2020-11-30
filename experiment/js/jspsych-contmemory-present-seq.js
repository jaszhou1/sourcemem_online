/**
 * jspsych-contmemory-present-seq
 *
 * A plugin to handle the display and response acquisition for
 * word--location pair presentation and location reproduction as part
 * of the online version of the continuous outcome source memory
 * experiment.
 */

jsPsych.plugins['contmemory-present-seq'] = (function() {
    var plugin = {};

    plugin.info = {
        name: 'contmemory-present-seq',
        description: 'Word--location presentation and location reproduction during encoding phase',
        parameters: {
            stimulus: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Stimulus',
                default: null,
                description: 'The word to be stored in memory'
            },
            angle: {
                type: jsPsych.plugins.parameterType.FLOAT,
                pretty_name: 'Angle',
                default: 0.0,
                description: 'The angle to be associated with the stimulus word'
            },
            angle_display_ms: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Stimulus angle display time (ms)',
                default: 1000,
                description: 'The time to present the stimulus angle before the stimulus word in milliseconds.'
            },
            word_display_ms: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Stimulus word display time (ms)',
                default: 1000,
                description: 'The time to present the stimulus word before the response in milliseconds.'
            },
            svg_size_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'SVG canvas size (px)',
                default: 600,
                description: 'The size of the SVG canvas element in pixels'
            },
            circle_buffer_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Circle radius buffer (px)',
                default: 80, // I have made the circle smaller so the word doesn't clip the 500*500 canvas. Ask simon about size
                description: 'The number of pixels between the circumference of the circle and the SVG canvas edge'
            },
            word_buffer_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Word display radius buffer (px)',
                default: 20,
                description: 'The number of pixels between the circumference of the circle and the textbox anchor'
            },
            fixation_length_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Length of each fixation cross arm (px)',
                default: 5,
                description: 'The length of the fixation cross in pixels'
            },
            angle_marker_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Radius of angle marker (px)',
                default: 10,
                description: 'The radius of the stimulus location marker in pixels'
            },
            calibration_marker_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Radius of the calibration area (px)',
                default: 8,
                description: 'The radius of the calibration area marker in pixels'
            },
            quick_trap_ms: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Shortest valid response time (ms)',
                default: 300,
                description: 'The upper bound of the quick response time trap in milliseconds'
            },
            slow_trap_ms: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Longest valid response time (ms)',
                default: 7000,
                description: 'The lower bound of the slow response time trap in milliseconds'
            },
            color: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Colour of stimuli',
                default: 'white',
                description: 'The colour of the stimuli to be displayed'
            }
        }
    };

    plugin.trial = function(display_element, trial) {
        // Start the timer for the trial.
        var start_trial = performance.now(),
            start_stimulus_angle = -1.0,
            start_stimulus_word = -1.0,
            start_response = -1.0,
            start_feedback = -1.0;

        // Declare all of the main display components.
        var svg_element = null,
            fixation_element = null,
            calibration_marker_element = null,
            response_circle_element = null,
            angle_marker_element = null,
            feedback_marker_element = null,
            feedback_text_element = null,
            stimulus_text_element = null;

        // Declare each of the trial components.
        var num_fast_attempts = 0,
            num_slow_attempts = 0,
            num_error_attempts = 0,
            stimulus_word = trial.stimulus,
            stimulus_angle = trial.angle,
            hitting_position = [0, 0],
            hitting_angle = null,
            angular_error = null,
            response_time = null,
            display_angle_time = null,
            display_word_time = null;

        // Variables for tracking the mouse position.
        var mouse_x = null,
            mouse_y = null;

        // Compute the constants for laying out the stimuli.
        const SVG_NAMESPACE = 'http://www.w3.org/2000/svg';
        const MIDPOINT_X = trial.svg_size_px / 2;
        const MIDPOINT_Y = trial.svg_size_px / 2;
        const CIRCLE_RADIUS_PX = (trial.svg_size_px / 2) - trial.circle_buffer_px;
        const WORD_RADIUS_PX = CIRCLE_RADIUS_PX + trial.word_buffer_px

        // Function to "normalise" an angle in radians (ensure it is
        // in the range of [0, 2pi]).
        var normalise_angle = function(angle) {
            while(angle > 2*Math.PI) {
                angle -= 2*Math.PI;
            }
            while(angle < 0) {
                angle += 2*Math.PI;
            }
            return angle;
        };

        // Function to compute the x part of the Cartesian coordinates
        // from a set of radial coordinate.
        var pol_to_cart_x = function(theta, rho) {
            return rho * Math.cos(theta) + MIDPOINT_X;
        };

        // Function to compute the y part of the Cartesian coordinates
        // from a set of radial coordinates.
        var pol_to_cart_y = function(theta, rho) {
            return rho * Math.sin(theta) + MIDPOINT_Y;
        };

        // Function to compute the radial coordinates from Cartesian
        // coordinates.
        var cart_to_pol = function(x, y) {
            var rho = Math.sqrt(x*x + y*y)
            var theta  = Math.atan2(y,x)
            return {
                rho: rho, theta: theta
            }
        };

        // Function to compute angular difference.
        var angular_difference = function(a, b) {
          diff = Math.atan2(Math.sin(a-b), Math.cos(a-b))
          return diff
          };

        // Function to check whether an angle is sufficiently close to
        // the target angle.
        var angle_within_limits = function(angle) {
            return Math.abs(angular_difference(trial.angle, angle)) <= Math.PI / 8;
        }

        // Function to check whether the mouse cursor's current
        // position is within an element's bounding box.
        var mouse_within_element = function(el) {
            var bbounds = el.getBoundingClientRect();
            if(mouse_x !== null && mouse_y !== null) {
                if(mouse_x >= bbounds.left && mouse_x <= bbounds.right && mouse_y >= bbounds.top && mouse_y <= bbounds.bottom) {
                    return true;
                }
            }
            return false;
        };

        // Create a circle and append it as an (invisible) child of
        // the parent SVG element.
        var create_and_append_circle = function(id, x_pos, y_pos, radius) {
            var res = document.createElementNS(SVG_NAMESPACE, 'circle');
            res.setAttribute('cx', x_pos);
            res.setAttribute('cy', y_pos);
            res.setAttribute('r', radius);
            res.style.color = trial.color;
            res.style.fill = trial.color;
            res.id = id;
            res.style.visibility = 'hidden';
            svg_element.appendChild(res);
            return res;
        };

        // Create a text element and append it as an (invisible) child
        // of the SVG element.
        var create_and_append_text = function(id, text, x_pos, y_pos, text_anchor) {
            var res = document.createElementNS(SVG_NAMESPACE, 'text');
            res.innerHTML = text;
            res.id = id;
            res.setAttribute('x', x_pos);
            res.setAttribute('y', y_pos);
            res.setAttribute('text-anchor', text_anchor);
            res.style.fill = trial.color;
            res.style.visibility = 'hidden';
            svg_element.appendChild(res);
            return res;
        };

        var calibration_display = function(return_to_marker) {
            console.log('Calibration display');

            // Make sure the feedback text is indicating people should
            // enter the calibration circle.
            if(return_to_marker) {
                feedback_text_element.innerHTML = 'Wait until cross appears before responding.';
            } else {
                feedback_text_element.innerHTML = 'Please place your cursor in the small circle.';
            }
            feedback_text_element.setAttribute('y', MIDPOINT_Y - trial.calibration_marker_px - 10);


            // Set the non-calibration elements to visibility: hidden.
            fixation_element.style.visibility = 'hidden';
            angle_marker_element.style.visibility = 'hidden';
            feedback_marker_element.style.visibility = 'hidden';
            stimulus_text_element.style.visibility = 'hidden';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'visible';
            response_circle_element.style.visibility = 'visible';
            feedback_text_element.style.visibility = 'visible';

            // Add an event handler to switch when the mouse is inside
            // the calibration marker.
            calibration_marker_element.addEventListener('mouseenter',
                                                        calibration_circle_entered_no_restart);
        };

        var angle_display = function() {
            console.log('Stimulus angle display');
            // Set the non-calibration elements to visibility: hidden.
            fixation_element.style.visibility = 'hidden';
            feedback_marker_element.style.visibility = 'hidden';
            feedback_text_element.style.visibility = 'hidden';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'visible';
            angle_marker_element.style.visibility = 'visible';
            response_circle_element.style.visibility = 'visible';
            stimulus_text_element.style.visibility = 'hidden';

            // Add an event handler to switch when the mouse is inside
            // the calibration marker.
            calibration_marker_element.addEventListener('mouseenter',
                                                        calibration_circle_entered_no_restart);
        };

        var word_display = function() {
            console.log('Stimulus word display');

            // Set the non-calibration elements to visibility: hidden.
            fixation_element.style.visibility = 'hidden';

            feedback_marker_element.style.visibility = 'hidden';
            feedback_text_element.style.visibility = 'hidden';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'visible';
            angle_marker_element.style.visibility = 'hidden';
            response_circle_element.style.visibility = 'visible';
            stimulus_text_element.style.visibility = 'visible';

            // Add an event handler to switch when the mouse is inside
            // the calibration marker.
            calibration_marker_element.addEventListener('mouseenter',
                                                        calibration_circle_entered_no_restart);
        };

        var response_display = function() {
            console.log('Response display');

            // Set the non-calibration elements to visibility: hidden.
            calibration_marker_element.style.visibility = 'hidden';
            angle_marker_element.style.visibility = 'hidden';
            feedback_marker_element.style.visibility = 'hidden';
            stimulus_text_element.style.visibility = 'hidden';
            feedback_text_element.style.visibility = 'hidden';

            // Set the calibration elements to visibility: visible.
            fixation_element.style.visibility = 'visible';
            response_circle_element.style.visibility = 'visible';
        };

        var feedback_display = function() {
            console.log('Feedback display');

            // Set the non-calibration elements to visibility: hidden.
            fixation_element.style.visibility = 'hidden';
            angle_marker_element.style.visibility = 'hidden';
            stimulus_text_element.style.visibility = 'hidden';

            // Draw hitting angle
            feedback_marker_element.style.visibility = 'visible';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'visible';
            response_circle_element.style.visibility = 'visible';
            feedback_text_element.style.visibility = 'visible';

        };

        var verification_display = function() {
            console.log('Verification display');
            console.log(hitting_angle)

            // Set the non-calibration elements to visibility: hidden.
            fixation_element.style.visibility = 'hidden';
            angle_marker_element.style.visibility = 'hidden';
            stimulus_text_element.style.visibility = 'hidden';

            // Draw hitting angle
            feedback_marker_element.style.visibility = 'visible';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'hidden';
            response_circle_element.style.visibility = 'visible';
            feedback_text_element.style.visibility = 'hidden';

        };

        var set_coordinates = function(e) {
            var posx = 0;
	    var posy = 0;
	    if (!e) var e = window.event;
	    if (e.pageX || e.pageY) 	{
		posx = e.pageX;
		posy = e.pageY;
	    }
	    else if (e.clientX || e.clientY) 	{
		posx = e.clientX + document.body.scrollLeft
		     + document.documentElement.scrollLeft;
		posy = e.clientY + document.body.scrollTop
		     + document.documentElement.scrollTop;
	    }
	    mouse_x = posx;
            mouse_y = posy;
        }

        var set_hitting_position = function(e) {
            var posx = 0;
	    var posy = 0;
            var fixation_position = fixation_element.getBoundingClientRect();
            var fixation_midx = (fixation_position.right - fixation_position.width/2),
                fixation_midy = (fixation_position.bottom - fixation_position.height/2);
	    if (!e) var e = window.event;
	    if (e.pageX || e.pageY) 	{
		posx = e.pageX;
		posy = e.pageY;
	    }
	    else if (e.clientX || e.clientY) 	{
		posx = e.clientX + document.body.scrollLeft
		     + document.documentElement.scrollLeft;
		posy = e.clientY + document.body.scrollTop
		     + document.documentElement.scrollTop;
	    }
            hitting_position = [posx-fixation_midx, posy-fixation_midy];
        }

        // Set up all of the elements for the trial. First, clear
        // whatever is already in the container element.
        display_element.innerHTML = '';

        // Construct the SVG element that will hold the stimulus.
        svg_element = document.createElementNS(SVG_NAMESPACE, 'svg');
        svg_element.id = 'jspsych-contmemory-present';

        // Set the height and width of the SVG element.
        svg_element.setAttribute('height', trial.svg_size_px);
        svg_element.setAttribute('width', trial.svg_size_px);
        svg_element.setAttribute(
            'viewBox',
            '0 0 ' +
            trial.svg_size_px.toString() + ' ' +
            trial.svg_size_px.toString()
        );
        display_element.appendChild(svg_element);

        // Construct the response circle element.
        response_circle_element = create_and_append_circle('response-circle',
                                                           MIDPOINT_X, MIDPOINT_Y,
                                                           CIRCLE_RADIUS_PX);
        response_circle_element.classList.add('response-circle');

        // Construct the fixation cross element at the centre of the stimulus.
        fixation_element = document.createElementNS(SVG_NAMESPACE, 'path');
        var path_description =
            'M ' // moveto with absolute coordinates
            + (MIDPOINT_X - trial.fixation_length_px).toString()
            + ',' + MIDPOINT_Y.toString() + ' '
        // lineto horizontal offset
            + 'h ' + (trial.fixation_length_px * 2).toString() + ' '
        // moveto with absolute coordinates
            + 'M ' + MIDPOINT_X.toString() + ','
            + (MIDPOINT_Y - trial.fixation_length_px).toString()
        // lineto with vertical offset
            + 'v ' + (trial.fixation_length_px * 2).toString();
        fixation_element.setAttribute('d', path_description);
        fixation_element.classList.add('fixation-cross');
        fixation_element.style.visibility = 'hidden';
        svg_element.appendChild(fixation_element);

        // Construct stimulus word element.
        stimulus_text_element = create_and_append_text('stimulus-text', trial.stimulus,
                                                       MIDPOINT_X,
                                                       MIDPOINT_Y - trial.calibration_marker_px - 10,
                                                       'middle');

        // Construct stimulus angle marker element.
        angle_marker_element = create_and_append_circle(
            'stim-angle-marker',
            CIRCLE_RADIUS_PX * Math.cos(trial.angle) + MIDPOINT_X,
            CIRCLE_RADIUS_PX * Math.sin(trial.angle) + MIDPOINT_Y,
            trial.angle_marker_px
        );
        angle_marker_element.classList.add('angle-marker');

        // Construct the calibration marker.
        calibration_marker_element = create_and_append_circle('calibration-marker',
                                                              MIDPOINT_X,
                                                              MIDPOINT_Y,
                                                              trial.calibration_marker_px);
        calibration_marker_element.classList.add('calibration-marker');

        // Construct feedback text.
        feedback_text_element = create_and_append_text('feedback-text', 'Too fast',
                                                       MIDPOINT_X, MIDPOINT_Y, 'middle');

        // Construct feedback marker element.
        feedback_marker_element = create_and_append_circle('feedback-marker',
                                                           MIDPOINT_X, MIDPOINT_Y,
                                                           trial.angle_marker_px);


        // The procedure to be called at the end of a trial.
        var end_trial_handle = function() {
            // Kill any remaining setTimeout handlers.
            jsPsych.pluginAPI.clearAllTimeouts();

            // Construct the trial data structure to be handed to jsPsych.
            var trial_data = {
                num_fast_attempts: num_fast_attempts,
                num_slow_attempts: num_slow_attempts,
                num_error_attempts: num_error_attempts,
                stimulus_word: stimulus_word,
                stimulus_angle: stimulus_angle,
                hitting_position: hitting_position,
                hitting_angle: hitting_angle,
                angular_error: angular_error,
                response_time: response_time,
                display_angle_time: display_angle_time,
                display_word_time: display_word_time
            };

            //console.log(trial_data);

            // Indicate to jsPsych that the trial is over.
            jsPsych.finishTrial(trial_data);
        };

        // The event listener for exiting the response circle.
        var response_circle_exited = function(e) {
            console.log("Response circle exited");
            // Ignore if we've entered the fixation element.
            if(e.relatedTarget === fixation_element) {
                return;
            }

            // Remove the event listener.
            response_circle_element.removeEventListener('mouseout', response_circle_exited);

            // Compute position.
            set_hitting_position(e);
            hitting_angle = cart_to_pol(hitting_position[0], hitting_position[1]).theta;
            hitting_angle = normalise_angle(hitting_angle);
            angular_error = angular_difference(trial.angle, hitting_angle); // Find the signed angular distance between the target and the response
            console.log(hitting_position[0], hitting_position[1]);
            present_feedback();
        };

        // The event listener for entering the calibration circle.
        var calibration_circle_entered = function(e) {
            console.log('Calibration element entered');
            // Remove the event listener.
            calibration_marker_element.removeEventListener('mouseenter', calibration_circle_entered);

            // Update the position (so that the intersection query works even though we're
            // in a child of the response circle, not the response circle itself.
            set_coordinates(e);

            present_angle();
        };

        // The event listener for entering the calibration circle when
        // we don't want to restart the presentation.
        var calibration_circle_entered_no_restart = function(e) {
            console.log('Calibration element entered (no restart)');
            // Remove the event listener.
            calibration_marker_element.removeEventListener('mouseenter', calibration_circle_entered_no_restart);

            // Update the position (so that the intersection query works even though we're
            // in a child of the response circle, not the response circle itself.
            set_coordinates(e);
        };


        // The event listener to track the mouse position.
        var response_circle_moved = function(e) {
            set_coordinates(e);
        };

        // A routine for the presentation of each stage of the experiment.
        var present_feedback = function() {
            // Set the feedback time.
            start_feedback = performance.now();
            response_time = start_feedback - start_response;

            // Check whether the response time is valid.
            if(response_time < trial.quick_trap_ms) {
                num_fast_attempts++;

                feedback_text_element.innerHTML = 'Too fast';
                feedback_marker_element.setAttribute('cx', hitting_position[0] + MIDPOINT_X);
                feedback_marker_element.setAttribute('cy', hitting_position[1] + MIDPOINT_Y);
                feedback_display();

                // After a delay, begin the trial again.
                jsPsych.pluginAPI.setTimeout(function() {
                    begin_presentation();
                }, 2000);
                return;
            }

            // Check whether the response time is valid.
            if(response_time > trial.slow_trap_ms) {
                num_slow_attempts++;

                feedback_text_element.innerHTML = 'Too slow';
                feedback_marker_element.setAttribute('cx', hitting_position[0] + MIDPOINT_X);
                feedback_marker_element.setAttribute('cy', hitting_position[1] + MIDPOINT_Y);
                feedback_display();

                // After a delay, begin the trial again.
                jsPsych.pluginAPI.setTimeout(function() {
                    begin_presentation();
                }, 2000);
                return;
            }

            // Check whether the angle is valid.
            if(!angle_within_limits(hitting_angle)) {
                num_error_attempts++;

                feedback_text_element.innerHTML = 'Too distant';
                feedback_marker_element.setAttribute('cx', hitting_position[0] + MIDPOINT_X);
                feedback_marker_element.setAttribute('cy', hitting_position[1] + MIDPOINT_Y);
                feedback_display();

                // After a delay, begin the trial again.
                jsPsych.pluginAPI.setTimeout(function() {
                    begin_presentation();
                }, 2000);
                return;
            }

            // Draw the computed hitting angle, and print to console for debugging
            feedback_marker_element.setAttribute('cx', hitting_position[0] + MIDPOINT_X);
            feedback_marker_element.setAttribute('cy', hitting_position[1] + MIDPOINT_Y);
            verification_display();

            // After a delay, end the trial.
            jsPsych.pluginAPI.setTimeout(function() {
              end_trial_handle();
            }, 2000);
        };

        var present_angle = function() {
            // Set up the stimulus display elements.
            angle_display();

            // Set the stimulus time.
            start_stimulus_angle = performance.now();

            // Set up the stimulus display to be removed.
            jsPsych.pluginAPI.setTimeout(function() {
                present_word();
            }, trial.angle_display_ms);
        };

        var present_word = function() {
            // Set up the stimulus display elements.
            word_display();

            // Set the stimulus time.
            start_stimulus_word = performance.now();

            // Calculate angle display time
            display_angle_time = start_stimulus_word - start_stimulus_angle;

            // Set up the stimulus display to be removed.
            jsPsych.pluginAPI.setTimeout(function() {
                present_response();
            }, trial.word_display_ms);
        };

        var present_response = function() {
            // If we're not inside the calibration marker element
            // bounding box, then go to calibration marker.
            if(!mouse_within_element(calibration_marker_element)) {
                console.log('At response, mouse not within calibration marker.');
                begin_presentation(true);
                return;
            }

            // Set up the response display elements.
            response_display();

            // Set the response timestamp.
            start_response = performance.now();

            // Calculate word display time
            display_word_time = start_response - start_stimulus_word;

            // Set up the response circle.
            response_circle_element.addEventListener('mouseout',
                                                     response_circle_exited);
        };

        var begin_presentation = function(return_to_marker) {
            console.log('Presentation begun');

            if(mouse_within_element(calibration_marker_element)) {
                // If we're already inside the calibration marker, go
                // straight to the stimulus display.
                console.log('At begin_presentation, mouse within calibration marker.');
                angle_display();
            } else {
                // Show the calibration marker and text.
                calibration_display(return_to_marker);
            }

            // Add an event handler to the response circle. Because
            // this event listener is registered to the same (named)
            // function, it won't get bound twice if begin_presentation
            // is called multiple times.
            response_circle_element.addEventListener('mousemove',
                                                     response_circle_moved);

            // Add an event handler to switch when the mouse is inside
            // the calibration marker.
            calibration_marker_element.addEventListener('mouseenter',
                                                        calibration_circle_entered);
        };

        // Set the stage for the calibration section.
        begin_presentation(false);
    };

    return plugin;
})();
