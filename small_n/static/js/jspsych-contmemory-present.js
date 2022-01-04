/**
 * jspsych-contmemory-present
 *
 * A plugin to handle the display and response acquisition for
 * word--location pair presentation and location reproduction as part
 * of the online version of the continuous outcome source memory
 * experiment.
 */

jsPsych.plugins['contmemory-present'] = (function() {
    var plugin = {};

    plugin.info = {
        name: 'contmemory-present',
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
            stimulus_display_ms: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Stimulus display time (ms)',
                default: 2000,
                description: 'The time to present the stimulus before the response in milliseconds.'
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
            start_stimulus = -1.0,
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

        // Function for positioning the stimulus word text element.
        // NOTE: For getBBox to work correctly, the text element must
        // already be a child of the SVG element. Just ensure that it is
        // hidden.
        var position_text = function(text_element, target_angle,
                                     word_radius) {
            // A note on positioning: we divide the circle into eight
            // equal sized sectors (cardinal directions and
            // intercardinal directions) because there are eight anchors
            // on a text box. The coordinates of a circle start from
            // the eastern point and go clockwise.

            // Sector / Circle / Closest text box anchor:
            // 0      / E      / (Mid-)left
            // 1      / SE     / Upper left
            // 2      / S      / Top (middle)
            // 3      / SW     / Upper right
            // 4      / W      / (Mid-)right
            // 5      / NW     / Lower right
            // 6      / N      / Bottom (middle)
            // 7      / NE     / Lower left

            // The number of sectors (8) matches the number of handles on a text box.
            const NUM_SECTORS = 8;
            const SECTOR_ANGLE = 2 * Math.PI / NUM_SECTORS;
            const LOWER_ANGLE = [...Array(NUM_SECTORS).keys()].map(i => i * SECTOR_ANGLE - SECTOR_ANGLE/2.0);
            const ANCHOR_X = pol_to_cart_x(target_angle, word_radius);
            const ANCHOR_Y = pol_to_cart_y(target_angle, word_radius);
            const WORD_DIMS = text_element.getBBox();
            const WORD_HEIGHT = WORD_DIMS.height;
            const WORD_WIDTH = WORD_DIMS.width;
            // just hardcode the coordinate offset
            const WORD_X_HANDLE = [0, 0, -WORD_WIDTH/2, -WORD_WIDTH, -WORD_WIDTH, -WORD_WIDTH, -WORD_WIDTH/2, 0];
            const WORD_Y_HANDLE = [WORD_HEIGHT/4, WORD_HEIGHT/2, WORD_HEIGHT/2, WORD_HEIGHT/2, WORD_HEIGHT/4, 0, 0, 0];
            // NB: The WORD_Y_HANDLE *ought* to be moved to
            // WORD_HEIGHT/2 and WORD_HEIGHT, but this seems to
            // produce words that are double the Y distance away.

            // Ensure the angle is correctly normalised to be between 0 and 2*pi.
            target_angle = normalise_angle(target_angle);

            // Find the first index over the "lower angle" for a
            // sector and under the "lower angle" for the next sector.
            for(var i = 0; i < NUM_SECTORS - 1; i++) {
                if((target_angle >= LOWER_ANGLE[i]) && (target_angle < LOWER_ANGLE[i+1])) {
                    break;
                }
            }

            text_element.setAttribute('x', WORD_X_HANDLE[i] + ANCHOR_X);
            text_element.setAttribute('y', WORD_Y_HANDLE[i] + ANCHOR_Y);
            text_element.setAttribute('text-anchor', 'start');
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
            feedback_text_element.setAttribute('y', MIDPOINT_Y - trial.calibration_marker_px - 5);


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

        var stimulus_display = function() {
            console.log('Stimulus display');
            // Set the non-calibration elements to visibility: hidden.
            fixation_element.style.visibility = 'hidden';

            feedback_marker_element.style.visibility = 'hidden';
            feedback_text_element.style.visibility = 'hidden';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'visible';
            angle_marker_element.style.visibility = 'visible';
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
            feedback_marker_element.style.visibility = 'hidden';
            stimulus_text_element.style.visibility = 'hidden';

            // Set the calibration elements to visibility: visible.
            calibration_marker_element.style.visibility = 'visible';
            response_circle_element.style.visibility = 'visible';
            feedback_text_element.style.visibility = 'visible';

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
                                                       MIDPOINT_X, MIDPOINT_Y, 'middle');
        position_text(stimulus_text_element, trial.angle, WORD_RADIUS_PX);

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
                display_angle_time: display_time,
                display_word_time: display_time
            };

            //console.log(trial_data);

            // Indicate to jsPsych that the trial is over.
            jsPsych.finishTrial(trial_data);
        };

        // The event listener for exiting the response circle.
        var response_circle_exited = function(e) {
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
            angular_error = angular_difference(trial.angle, hitting_angle);
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

            present_stimulus();
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
                feedback_marker_element.setAttribute('cx', hitting_position[0]);
                feedback_marker_element.setAttribute('cy', hitting_position[1]);
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
                feedback_marker_element.setAttribute('cx', hitting_position[0]);
                feedback_marker_element.setAttribute('cy', hitting_position[1]);
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
                feedback_marker_element.setAttribute('cx', hitting_position[0]);
                feedback_marker_element.setAttribute('cy', hitting_position[1]);
                feedback_display();

                // After a delay, begin the trial again.
                jsPsych.pluginAPI.setTimeout(function() {
                    begin_presentation();
                }, 2000);
                return;
            }

            // End the trial.
            end_trial_handle();
        };

        var present_stimulus = function() {
            // Set up the stimulus display elements.
            stimulus_display();

            // Set the stimulus time.
            start_stimulus = performance.now();

            // Set up the stimulus display to be removed.
            jsPsych.pluginAPI.setTimeout(function() {
                present_response();
            }, trial.stimulus_display_ms);
        };

        var present_response = function() {
            // If we're not inside the calibration marker element
            // bounding box, then go to calibration marker.
            if(!mouse_within_element(calibration_marker_element)) {
                console.log('At present response, mouse outside calibration marker.');
                begin_presentation(true);
                return;
            }

            // Set up the response display elements.
            response_display();

            // Set the response timestamp.
            start_response = performance.now();

            // Calculate stimulus display time
            display_time = start_response - start_stimulus;

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
                stimulus_display();
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
