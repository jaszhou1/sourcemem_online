function [qlines_res, theta_vector] = joint_dist_to_qlines(time_vector, ...
    theta_vector, joint_density, time_quantiles, time_step)
% JOINT_DIST_TO_QLINES Compute the quantile lines from the pred. joint dist.
%   Generate the RT quantiles across all theta values from the predicted
%   joint distribution (outputted by the model).
%   

%% Normalise the distribution (multiplying in the time step and theta step).
n_theta_steps = size(joint_density, 1);
theta_step = 2 * pi / n_theta_steps;
n_time_steps = size(time_vector, 2);
joint_distribution = cumsum(joint_density, 2) .* time_step .* theta_step;
max_pr_by_theta = joint_distribution(:, n_time_steps) .* ...
    ones(1, n_time_steps);
cond_distribution = joint_distribution ./ max_pr_by_theta;

%% Preallocate the output matrix.
n_time_quantiles = size(time_quantiles, 2);
qlines_res = zeros(n_theta_steps, n_time_quantiles);

%% For each angle, compute the response time quantiles conditioned on that angle.
MIN_STABLE_PR = 0.025;
MAX_STABLE_PR = 0.975;
for i = 1:(n_theta_steps-1)  % The last row is not reflective of the dist
    cond_dist_i = cond_distribution(i, :);
    stable_indices = ...
        (cond_dist_i >= MIN_STABLE_PR & cond_dist_i <= MAX_STABLE_PR);
    stable_dist = cond_dist_i(stable_indices);
    stable_time = time_vector(stable_indices);
    if min(diff(stable_dist)) <= 0
        error 'Cannot compute distribution quantiles';
    end
    disp(i)
    disp(size(stable_dist))
    qlines_res(i, :) = interp1(stable_dist', stable_time', time_quantiles);
end

theta_vector = theta_vector(1:(n_theta_steps-1));

end
