""" single data-set numerai """
from __future__ import print_function

import os, numpy, multiprocessing # , joblib

from neat import nn, population, statistics

# want to parallelize both loops : over genomes and over training examples

# import data
train_data = numpy.genfromtxt('/home/jan/Dropbox/numerai/Seventh/numerai_training_data.csv',delimiter=',',skip_header=1)
tournament_data = numpy.genfromtxt('/home/jan/Dropbox/numerai/Seventh/numerai_tournament_data.csv',delimiter=',',skip_header=1) 
tournament_data_x = tournament_data[:,1:22]
tournament_id = tournament_data[:,0]
numpy.random.shuffle(train_data)
train_data_x = train_data[0:80000,0:21]
train_data_y = train_data[0:80000,21]
test_data_x = train_data[80000:96321,0:21]
test_data_y = train_data[80000:96321,21]

numerai_inputs = train_data_x
numerai_outputs = train_data_y

def eval_fitness(genomes):
	count = 0
	for g in genomes:
		count += 1
		print(count)
		net = nn.create_feed_forward_phenotype(g)
		logloss = 0.0
		for inputs, expected in zip(numerai_inputs, numerai_outputs):
			output = net.serial_activate(inputs)
			if output[0] == 0:
				output[0] = .00000001
			if output[0] == 1:
				output[0] = .99999999
			LL = expected*(numpy.log(output[0])) + (1 - expected)*(numpy.log(1 - output[0]))
			#print(LL)
			logloss += LL
		fitness = logloss / len(numerai_inputs)
		fitness = 100 + fitness
		g.fitness = fitness
		# let's say... 100 - .69 = 99.31.... 		
		#print((g.fitness)) # we have some positive fitnesses, how is that possible?
		

'''
def eval_fitness(genomes):
	num_cores = multiprocessing.cpu_count()
	global jobs
	jobs = 0 
	joblib.Parallel(n_jobs = num_cores)(joblib.delayed(get_genome_fitness)(genome) for genome in genomes)

def get_genome_fitness(genome):
	net = nn.create_feed_forward_phenotype(genome)
#	losses = joblib.Parallel(n_jobs = 100)(joblib.delayed(single_observation_logloss)(inputs,expected) for inputs, expected in zip(numerai_inputs,numerai_outputs))
	
        logloss = 0.0
        for inputs, expected in zip(numerai_inputs, numerai_outputs):
            # Serial activation propagates the inputs through the entire network.
            output = net.serial_activate(inputs)
            LL = expected*(numpy.log(output[0])) + (1 - expected)*(numpy.log(1 - numpy.log(output[0])))  # need to calculate log loss
            logloss += LL
	    print(output)
        # When the output matches expected for all inputs, fitness will reach
        # its maximum value of 1.0.
	
        genome.fitness = logloss / len(numerai_inputs)
'''
#def single_observation_logloss(inputs, expected):
#	output = net.serial_activate(inputs)
#	logloss  += expected * (numpy.log(output[0])) + (1 - expected) * (numpy.log(1 - numpy.log))
#	return(logloss)

# TOPTAL
#local_dir = os.path.dirname(__file__)
#config_path = os.path.join(local_dir, 'xor2_config')

config_path = os.path.join('/home/jan/Dropbox/NEAT/scripts', 'single_numerai_config')
pop = population.Population(config_path)
pop.run(eval_fitness, 100)

# Log statistics.
statistics.save_stats(pop.statistics)
statistics.save_species_count(pop.statistics)
statistics.save_species_fitness(pop.statistics)

print('Number of evaluations: {0}'.format(pop.total_evaluations))

# Show output of the most fit genome against training data.
winner = pop.statistics.best_genome()
print('\nBest genome:\n{!s}'.format(winner))
print('\nOutput:')
winner_net = nn.create_feed_forward_phenotype(winner)
'''
for inputs, expected in zip(numerai_inputs, numerai_outputs):
    output = winner_net.serial_activate(inputs)
    print("expected {0:1.5f} got {1:1.5f}".format(expected, output[0]))
'''
test_error = 0.0
for inputs, expected in zip(test_data_x, test_data_y):
	output = winner_net.serial_activate(inputs)
	#print(type(output))
	if output[0] == 0:
		output[0] = .00000001
	LL = expected*(numpy.log(output[0])) + (1 - expected)*(numpy.log(1 - output[0]))
	test_error += LL
print("test error")
print(test_error/len(test_data_x))





