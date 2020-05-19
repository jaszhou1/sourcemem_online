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
            svg_size_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'SVG canvas size (px)',
                default: 500,
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
            }
        }
    };

    plugin.trial = function(display_element, trial) {
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
        
        // Function for positioning the stimulus word text element.
        // NOTE: For getBBox to work correctly, the text element must
        // already be a child of the SVG element. Just ensure that it is
        // hidden.
        var position_text = function(text_element, target_angle, word_radius, centre_x, centre_y) {
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
            const ANCHOR_X = word_radius * Math.cos(target_angle) + centre_x;
            const ANCHOR_Y = word_radius * Math.sin(target_angle) + centre_y;
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

        // Set up all of the drawing elements (albeit hidden) for the entire trial.
        
        // Clear whatever is already in the container element.
        display_element.innerHTML = '';

        // Compute the constants for laying out the stimuli.
        const SVG_NAMESPACE = 'http://www.w3.org/2000/svg';
        const MIDPOINT_X = trial.svg_size_px / 2;
        const MIDPOINT_Y = trial.svg_size_px / 2;
        const CIRCLE_RADIUS_PX = (trial.svg_size_px / 2) - trial.circle_buffer_px;
        const WORD_RADIUS_PX = CIRCLE_RADIUS_PX + trial.word_buffer_px

        // Construct the SVG element that will hold the stimulus.

        var svg_element = document.createElementNS(SVG_NAMESPACE, 'svg');
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

        // Construct the circle within the stimulus.
        var circle_element = document.createElementNS(SVG_NAMESPACE, 'circle');
        circle_element.setAttribute('cx', MIDPOINT_X);
        circle_element.setAttribute('cy', MIDPOINT_Y);
        circle_element.setAttribute('r', CIRCLE_RADIUS_PX);
        circle_element.classList.add('stimulus-circle');
        svg_element.appendChild(circle_element);

        // Construct the fixation cross at the centre of the stimulus.
        var fixation_element = document.createElementNS(SVG_NAMESPACE, 'path');
        var path_description = ''; // we're doin this the old postscript way...
        path_description = 'M ' // moveto with absolute coordinates
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
        svg_element.appendChild(fixation_element);

        // Construct hidden word stimulus to work out dimensions of textbox
        var text_element = document.createElementNS(SVG_NAMESPACE, 'text');
        text_element.innerHTML = trial.stimulus;
        text_element.style.visibility = 'hidden';
        display_element.appendChild(svg_element);
        svg_element.appendChild(text_element);
        
        position_text(text_element, trial.angle, WORD_RADIUS_PX, MIDPOINT_X, MIDPOINT_Y);

        text_element.style.visibility = 'visible';





        var marker_element = document.createElementNS(SVG_NAMESPACE, 'circle');
        marker_element.setAttribute('cx', CIRCLE_RADIUS_PX * Math.cos(trial.angle) + MIDPOINT_X);
        marker_element.setAttribute('cy', CIRCLE_RADIUS_PX * Math.sin(trial.angle) + MIDPOINT_Y);
        marker_element.setAttribute('r', 10);
        svg_element.appendChild(marker_element);

        // The way to get the size of the word is to append the text svg element
        // when it is is visibility: hidden, compute the bounding box size, reposition
        // the text based on the bounding box size, and then switch visibility: visible.
        //word_element.style.visibility = 'hidden';

          // First work out the quadrant that the angle is in.

          // Portion up the circle


          // Work out the dimensions of the text bounding box.

          // Based on the quadrant, set the text-anchor attribute.

          // Set the cartesian coordinates of the word element.

        // Append the SVG to the container node.
        display_element.appendChild(svg_element);
        console.log(trial.angle);


    };

    return plugin;
})();
