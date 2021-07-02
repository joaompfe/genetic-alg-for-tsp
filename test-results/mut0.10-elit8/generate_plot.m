function generate_plot(tsp, crossover, pop_size)
% Directory to save plots
dir = "./figs/";

% Setup the Import Options and import the data
opts = delimitedTextImportOptions("NumVariables", 4);

% Specify range and delimiter
opts.DataLines = [1, Inf];
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["i", "j", "pop_avg_fitness", "best_distance"];
opts.VariableTypes = ["double", "double", "double", "double"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Import the data
file_path = "pop" + pop_size +"-" +crossover + "-" + tsp +".csv";
data = readtable(file_path, opts);

% Clear temporary variables
clear opts

% Tests data average
avg_data = grpstats(data, 'j');

% Plot
hold off
plot(2500-avg_data.j, 1./avg_data.mean_pop_avg_fitness);
hold on
plot(2500-avg_data.j, avg_data.mean_best_distance);

% Plot appearence
switch tsp
    case "berlin52"
        ylim([7500, 30000])
    case "eil51"
        ylim([400, 1800])
    case "eil76"
        ylim([500, 2600])
    case "pr76"
        ylim([100000, 600000])
    case "rat99"
        ylim([1200, 9000])
end

% Plot title, legends, label
legend({'Média da população', 'Melhor indivíduo'});
title(tsp+", "+crossover+", pop\_size="+pop_size+" média de 10 testes");
ylabel('Distância TSP')
xlabel('Iterações')

% Save plot
fig_name = dir +"pop"+ pop_size +"-"+ crossover +"-"+ tsp;
saveas(gcf, fig_name, 'png');
saveas(gcf, fig_name, 'svg');

close(gcf);
