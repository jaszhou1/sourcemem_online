/**
 * jspsych-math-distractor
 * Originally coded by Adam Osth for jspsych [Version Number?]
 * Adapted for 6.1.0 by Jason Zhou (jasonz1@student.unimelb.edu.au)
 *
 * plugin for running a arithmetic distractor task
 *
 *
 **/


jsPsych.plugins['math-distractor'] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'math-distractor',
    description: '',
    parameters:{
      duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Distractor duration',
        default: 10,
        description: 'How long to run the distractor phase before it ends.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    // Time handlers
    var setTimeoutHandlers = [];

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    //trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    var rts = [];
    var num_a = [];
    var num_b = [];
    var num_c = [];
    var sums = [];
    var subj_responses = [];
    var rts = [];
    var tbox = '<div id = "stim"><p id="inst">1 = TRUE, 0 = FALSE</p>';

    // function to handle responses by the subject
    var after_response = function(info) {
      console.log('Response')
    };


    // Function to generate random ints
    function randomInt(min,max) {
      return Math.floor(Math.random() * (max - min)) + min;
    };


    var first_trial = function() {

      // setup questions
      var nums = [randomInt(0,10), randomInt(0,10), randomInt(0,10)];
      var sum_nums = nums[0] + nums[1] + nums[2];
      var nums2 = randomInt(-1,1);
      var presented_sum = sum_nums + nums2;
      var prob = '<div id="stim">' + nums[0].toString() + ' + ' + nums[1].toString() + ' + ' + nums[2].toString() + ' = ' + presented_sum.toString() + '</div>';
      display_element.innerHTML = prob + tbox;

      // log the new problem
      num_a.push(nums[0]);
      num_b.push(nums[1]);
      num_c.push(nums[2]);
      sums.push(presented_sum);

      jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: gen_trial,
        valid_responses: [48,49],
        rt_method: 'performance',
        persist: false,
        allow_held_key: false
      });
    };

    // gen_trial called after!
    var gen_trial = function(response) {

      // clear screen
      display_element.innerHTML = '';

      //display_element.querySelector('#jspsych-single-stim-stimulus').className += ' responded';
      if (response['key'] == 49){
        resp = 1;
      }
      else if (response['key'] == 48){
        resp = 0;
      }
      console.log('response');
      console.log(typeof(response));
      console.log(response);
      subj_responses.push(resp);
      rts.push(response['rt']);

      // 250 ms delay before the next problem appears
      currentTime = (new Date()).getTime();
      var t3 = setTimeout(function(){
        var nums = [randomInt(0,10), randomInt(0,10), randomInt(0,10)];
        var sum_nums = nums[0] + nums[1] + nums[2];
        var nums2 = randomInt(-1,1);
        var presented_sum = sum_nums + nums2;
        var prob = '<div id="stim">' + nums[0].toString() + ' + ' + nums[1].toString() + ' + ' + nums[2].toString() + ' = ' + presented_sum.toString() + '</div>';
        display_element.innerHTML = prob + tbox;

        // log the new problem
        num_a.push(nums[0]);
        num_b.push(nums[1]);
        num_c.push(nums[2]);
        sums.push(presented_sum);

        jsPsych.pluginAPI.getKeyboardResponse({
          callback_function: gen_trial,
          valid_responses: [48,49],
          rt_method: 'performance',
          persist: false,
          allow_held_key: false
        });
      }, 250); // this controls the ISI in the math task
      setTimeoutHandlers.push(t3);
    };

    var end_trial = function() {
      // kill any remaining setTimeout handlers
      for (var i = 0; i < setTimeoutHandlers.length; i++) {
        clearTimeout(setTimeoutHandlers[i]);
        // kill keyboard listeners
        jsPsych.pluginAPI.cancelAllKeyboardResponses();
      }

      // clear the display
      display_element.innerHTML = '';

      // gather the data to store for the trial
      var trial_data = {
        "rt": rts,
        "num1": num_a,
        "num2": num_b,
        "num3": num_c,
        "sums": sums,
        "subj_responses": subj_responses,
        "rts": rts
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };

    first_trial();

    var startTime = (new Date()).getTime();

    if (trial.duration > 0) {
      var t2 = setTimeout(function() {
        end_trial();
      }, trial.duration);
      setTimeoutHandlers.push(t2);
    }

  };


  return plugin;
})();
