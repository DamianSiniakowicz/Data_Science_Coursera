% Review

% What is Linear Regression? How do we use it, when do we use it?

% Linear Regression 

% you have a bunch of data, 
% each data point has values for several different features
% one of these features is called the target feature
% In Linear Regression we train our model using data that includes the
    % target feature
% Making models based on data that have the target feature is called supervised learning 
% our goal is to predict the target value of data points whose target value
    % is not available
% the model is a function that takes the new data point's feature vector as
    % input, 
% it outputs a linear combination of the feature vector
% the output represents the model's prediction of the target variable's
    % value
% the main work of linear regression is finding the model's parameters
    % the weights of the linear combination of features
% this is done by finding a parameter vector which minimizes a cost
    % function
% the cost function takes predicted target values and observed target
    % values as input
% the output is a quantified measure of how close the prediction was
% for most cost functions, a lower value means a better prediction
% In Linear Regression the cost function is 
    % the squared difference between the predicted and observed target
        % values
% The cost function is minimized by an algorithm called gradient descent
% Gradient Descent takes four inputs : initial parameters, 
    % a learning rate called alpha that determines the magnitude of parameter updates
    % a cost function 
    % an epsilon, if an iteration of gradient descent changes the cost
        % by less than epsilon, we output the parameters. 
% How it works
    % at each iteration, gradient descent updates the model's parameters
    % then it checks to see if the difference of the cost of the old and new sets
    % of parameters is less than epsilon.
    % if the difference is less than epsilion, the new parameters are
    % returned as the model's parameters, ie. we break the loop 
    % otherwise the parameters are updated in a new iteration of GD
% How gradient descent changes each parameter in a single update
    % for each parameter
    % from the value of the old parameter, 
        % it subtracts
            % the product of the learning rate and the average of
            % the sum of the following expression evaluated for each feature vector
                % the partial derivative of the cost function,
                    % with respect the parameter we are currently updating
### QUESTION : what is the precise value of the partial derivative
### is it (1/m)*(obs-y)*x_j
% I will now speak more generally about prediciton
% predicted value are the output of a hypothesis function
    % which takes a parameter vector and a feature vector as input
    % in linear regression the prediction equals the dot product of the input vectors

% The Logistic Regression hypothesis function is the sigmoid function applied to the 
    % output of the Linear Regression hypothesis function 
        % (the dot-product of the parameter vector of a model, and feature vector of the data point)
    % The sigmoid function takes a real number as input and returns a
        % number between 0 and 1. It is described as 1 / (1 + e^-z)
    % Conveniently, a probability is also real number between 0 and 1,
        % so we can treat the log-reg hypothesis function as a function that 
        % takes a feature vector and a parameter vector of equal length as input
        % and returns a probability
    % Therefore, we use it to describe the probability that an event has
        % occurred, or the degree of certainity in our preference for one action over another.
        % ex. on numerai, the event is: buy, it's complement is don't buy, or
        % maybe sell. 
        % the preference in favor of buy is p, sell is q = 1 - p
    % It can also be used in situations with several possible outcomes. 
        % In this siutation we perform logisitic regression as many times
        % as there are possible outcomes, or classes
        % in each regression we define the event as the occurence of one
            % particular class. The complement is the non-occurrence of
            % that particular class, ie. the occurrence of any other class.
        % We now have a measure of the subjective preference for each class
        % over all the others. # would additional pair-wise comps makesense
    % Although the hypothesis function outputs real numbers between 0 and 1
        % EXCLUDING 0 and 1, the actual target values for any data point are 
        % exactly 0 or exactly 1
    % Our model's error is calculated using a function called logarithmic
        % loss
    % Log-Loss is a piecewise function
        % when the target value is 1, log_loss = -log(prediction)
        % when the target value is 0, log_loss = -log(1-prediction)
    % Our parameters are updated by our old friend Gradient Descent, the 
        % same dude who updated our parameter vectors when we were doing
        % linear regression. He hooks you up with better parameters!
    % How Gradient Descent works, revisited
        % The jth parameter of our new model equals
        % the jth parameter of our old model minus
        % alpha * the derivative of the cost function evaluated at the
        % basic dot-product hypothesis function's output with respect to 
        % the jth parameter
        # something like sum of (pred_obs_i - acutal)*jth_feature_ith_obs
    % When we have updated our parameters max_iters times, or when the
    % change in cost function from one iteration to the next is less than
    % epsilon, GD calls it a day and returns the model's parameters
    
    % Neural Networks
    % they learned from logistic regression and then stepped shit up to another level
    % Neural Networks alternate between performing a single matrix vector multiplication, 
        % and applying the sigmoid function to the resulting vector of each multiplication
    % They are given a set of matrices, the weights, 
    % and an intial vector, the feature vector for a data point,
    % they output a vector of probabilities, 
    % the vector can be of length 1 or million, 
        % note that a length 2 vector is redundant because p = 1-q
    % Essentially we are taking linear combinations of linear combinations of ...
        % of the feature vector.
    % The neural network's cost function is usually log-loss.
    % The mechanism for updating the model parameters,     
        % the matrices of weights
    % is gradient descent
    % the update step is performed as follows
        % each weight is updated by taking it's old value and subtracting
        % the learning rate times the gradient 
        % the gradient of the ijth weight is calculated as follows
        % it is the sum of the gradients of the ij_th weight over all
        % observations divided by the number of observations
        % the gradient of a single weight for a single observation,
            % we shall call it the gradient of the ijth parameter of the
            % nth parameter matrix
            % equals the product of
            % jth feature of the feature vector fed into the nth matrix
                % multiplication
            % and the ith feature of the delta vector located after the nth
                % matrix
            % the nth delta vector is a vector of the same dimension as the
                % feature vector outputted by the nth matrix multiplication
            % it is calculated as follows
                % the delta vector corresponding to the output of the
                    % neural network
                % equals the neural network's output vector for the kth
                % observation - the observed target vector for the kth
                % observation
                % Delta Vectors corresponding to feature vectors outputted
                    % by matrix multiplications other than the last
                    % are calculated as follows
                % The kth element of the delta vector equals
                    % the product of
                    % the linear combination of the preceeding delta vector
                    % using the weights the kth element of the forward prop 
                    % feature vector applied to each of the proceeding
                    % elements of the next feature vector
                    % AND THE
                    % derivative of the sigmoid function
                    % evaluated at the pre-sigmoidal of the kth element
                    % of the feature vector passed into the nth matrix
                    % multiplication
        % The updating stops when a certain number of iterations has been
        % completed, or when the cost change between two iterations is
        % less than epsilon
   % 
                
    
    % Model creation done with 3 functions
    % a function to calculate a hypothesis
    % a function to calculate cost
    % a function to update the parameters
    
    % functions are people. Models are people.
    % Mathematical objects are people
    % what would a neural network with 2 output nodes represent
    % what if the outcomes were not disjoint?
    % Linear combinations can represent products. 
        % ex. 2y + 3z... y = 1.5, z = 1
    % Note : gradient descent takes : 
    % cost function, hypothesis function, initial parameters
    % params -> hypothesis -> cost -> update -> it's a loop
    % positives and negatives in a neural net parameter update
    
    