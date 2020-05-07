/**
 * jspsych-contmemory-present
 *
 * A plugin to handle the display and response acquisition for
 * word--location pair presentation and location reproduction as part
 * of the online version of the continuous outcome source memory
 * experiment.
 */

jsPsych.plugins["contmemory-present"] = (function() {
    var plugin = {};

    plugin.info = {
        name: 'contmemory-present',
        description: 'Word--location presentation and location reproduction during encoding phase',
        parameters: {
        }
    };

    plugin.trial = function(display_element, trial) {
    };
})();
