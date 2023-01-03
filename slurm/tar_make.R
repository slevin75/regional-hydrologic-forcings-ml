#Load the command line arguments
arg = commandArgs(T)

source('_targets.R')

#p1 targets are all done in serial (1 core) and parallization is discouraged for data downloads
#some p2 targets can benefit from parallelization, but doing in serial for now
tar_make(starts_with(c('p1', 'p2')))

#these p3 targets do not build with clustermq. I'm not sure why.
tar_make('p3_period_of_record_plots_estimated')
tar_make('p3_period_of_record_plots_no_estimated')

#p3 and p4 targets all build better with clustermq parallelization
tar_make_clustermq(starts_with(c('p3', 'p4')), workers = as.numeric(arg[1]) - 1, log_worker = TRUE)

#all other targets either build better without clustermq or are parallelized internally to run with tar_make
tar_make()