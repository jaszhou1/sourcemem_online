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
                default: 70, // I have made the circle smaller so the word doesn't clip the 500*500 canvas. Ask simon about size
                description: 'The number of pixels between the circumference of the circle and the SVG canvas edge'
            },
            word_buffer_px: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Word display radius buffer (px)',
                default: 10,
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
        // Clear whatever is already in the container element.
        display_element.innerHTML = '';

        // Here are the constants for laying out the stimuli.
        const MIDPOINT_X = trial.svg_size_px / 2;
        const MIDPOINT_Y = trial.svg_size_px / 2;
        const CIRCLE_RADIUS_PX = (trial.svg_size_px / 2) - trial.circle_buffer_px;
        const WORD_RADIUS_PX = CIRCLE_RADIUS_PX + trial.word_buffer_px

        // Construct the SVG element that will hold the stimulus.
        var svg_namespace = 'http://www.w3.org/2000/svg';
        var svg_element = document.createElementNS(svg_namespace, 'svg');
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
        var circle_element = document.createElementNS(svg_namespace, 'circle');
        circle_element.setAttribute('cx', MIDPOINT_X);
        circle_element.setAttribute('cy', MIDPOINT_Y);
        circle_element.setAttribute('r', CIRCLE_RADIUS_PX);
        circle_element.classList.add('stimulus-circle');
        svg_element.appendChild(circle_element);

        // Construct the fixation cross at the centre of the stimulus.
        var fixation_element = document.createElementNS(svg_namespace, 'path');
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
        var text_element = document.createElementNS(svg_namespace, 'text');
        text_element.innerHTML = trial.stimulus;
        text_element.style.visibility = 'hidden';
        text_element.setAttribute('x', MIDPOINT_X);
        text_element.setAttribute('y', MIDPOINT_Y);
        var text_width = 0, text_height = 0;
        svg_element.appendChild(text_element);
         // Maybe I shouldn't be showing it here, but not sure how to get word dimensions otherwise?
         // Seems to need to "flip" and be a drawn object (albeit invisible) to use getBBbox
        display_element.appendChild(svg_element);
        var word_dims = text_element.getBBox();  // I don't actually believe height is 20 pixels. Is there some sort of buffer?

        // Cartesian Co-ordinates of the text stimulus, ignoring anchor points
        word_x = MIDPOINT_X + (WORD_RADIUS_PX * Math.cos(trial.angle))
        word_y = MIDPOINT_Y + (WORD_RADIUS_PX * Math.sin(trial.angle))

        // Set anchor point co-ordinates based on sector

        if (trial.angle == 0 || trial.angle == 2*Math.PI){
          word_y = word_y + word_dims.height/4 // Move anchor down a bit
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'start');

        } else if (0 < trial.angle && trial.angle <= Math.PI/4){
          word_y = word_y + word_dims.height/4 // Move anchor down a bit
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'start');

        } else if (Math.PI/4 < trial.angle && trial.angle < Math.PI/2){
          word_y = word_y + word_dims.height/2
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'start');

        } else if (trial.angle == Math.PI/2){
                    word_y = word_y + word_dims.height // Move anchor down
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'middle');

        }else if (Math.PI/2 < trial.angle && trial.angle <= 3*Math.PI/4){
          word_y = word_y + word_dims.height/2
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'end');

        } else if (3*Math.PI/4 < trial.angle && trial.angle < Math.PI){
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'end');

        } else if (trial.angle == Math.PI){
          word_y = word_y + word_dims.height/4 // Move anchor down a bit
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'end');

        } else if (Math.PI < trial.angle && trial.angle <= 5*Math.PI/4){
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'end');

        } else if (5*Math.PI/4 < trial.angle && trial.angle < 3*Math.PI/2){
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'end');

        } else if (trial.angle == 3*Math.PI/2){
          word_y = word_y - word_dims.height/4 // Move anchor up a bit
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'middle');

        } else if (3*Math.PI/2 < trial.angle && trial.angle <= 7*Math.PI/4){
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'start');
        } else {
          var word_element = document.createElementNS(svg_namespace, 'text');
          word_element.innerHTML = trial.stimulus;
          word_element.setAttribute('x', word_x);
          word_element.setAttribute('y', word_y);
          word_element.setAttribute('text-anchor', 'start');
        }

        svg_element.appendChild(word_element);

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
