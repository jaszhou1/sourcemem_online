function [qxq_res, theta_centres] = data_to_qxq(data, n_theta_bins, ...
    time_quantiles, min_rt_filter, max_rt_filter)
%DATA_TO_QXQ Generate quantile-by-quantile cells from observations
%   Generate the quantile-by-quantile cells from observations. First
%   we take theta (angle) quantiles of responding and then,
%   conditioning on each quantile, take the response time quantiles
%   within that bin. The theta quantile is put in the centre of that
%   quantile bin. Data here is a Nx2 matrix: N observations with the
%   angle of response in the first column and the response time in the
%   second column.

  %% Filter the dataset and compute the broad statistics for the data set.
  data = data(data(:, 2) >= min_rt_filter & data(:, 2) <= max_rt_filter, :);
  n_observations = length(data);
  n_time_quantiles = length(time_quantiles);

  %% Compute the theta bins.
  upper_bin_pr_edge = linspace(0, 1, n_theta_bins + 1);
  upper_bin_pr_edge = upper_bin_pr_edge(2:end);
  theta_bin_cumsize = round(n_observations * upper_bin_pr_edge);

  %% Preallocate the data for the bin sizes.
  bin_rt = zeros(ceil(n_observations / (n_time_quantiles - 1)), ...
      n_theta_bins); 
  bin_theta = zeros(1, n_theta_bins);
  bin_count = zeros(1, n_theta_bins);

  %% Allocate observations to each theta bin.
  [~, I] = sort(data(:, 1));  % sort by theta
  data(:, :) = data(I, :);

  j = 1;  % the current theta quantile bin
  k = 1;  % the current observation within the theta quantile bin
  for i = 1:n_observations
    % If the current observation is over the size of the current bin,
    % increment the bin and start allocating elements there.
    if i > theta_bin_cumsize(j)
      j = j + 1;
      k = 1;
    end

    theta_i = data(i, 1);
    bin_theta(j) = bin_theta(j) + theta_i;
    bin_rt(k, j) = data(i, 2);
    bin_count(j) = bin_count(j) + 1;
    
    k = k + 1;
  end

  theta_centres = bin_theta ./ bin_count;

  %% Compute RTs for each bin.
  qxq_res = zeros(n_time_quantiles, n_theta_bins);
  for j = 1:n_theta_bins
    rt_for_theta_bin = bin_rt(1:bin_count(j), j);
    rt_for_theta_bin = sort(rt_for_theta_bin);
    quantile_rts = ceil(length(rt_for_theta_bin) .* time_quantiles);
    qxq_res(:, j) = rt_for_theta_bin(quantile_rts);
  end
end
