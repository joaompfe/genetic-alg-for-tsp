function generate_plots(tsps, crossovers, pop_sizes)

for pop_size = pop_sizes
    for co = crossovers
        for tsp = tsps
            generate_plot(tsp, co, pop_size);
        end
    end
end

    
