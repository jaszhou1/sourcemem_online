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
                default: 10,
                description: 'The number of pixels between the circumference of the circle and the SVG canvas edge'
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
        // lineto vertical offset
                         + 'h ' + (trial.fixation_length_px * 2).toString() + ' '
        // moveto with absolute coordinates
                         + 'M ' + MIDPOINT_X.toString() + ','
                         + (MIDPOINT_Y - trial.fixation_length_px).toString()
        // lineto with horizontal offset
                         + 'v ' + (trial.fixation_length_px * 2).toString();
        
        fixation_element.setAttribute('d', path_description);
        fixation_element.classList.add('fixation-cross');
        svg_element.appendChild(fixation_element);
        // Append the SVG to the container node.
        display_element.appendChild(svg_element);  
    };

    return plugin;
})();

