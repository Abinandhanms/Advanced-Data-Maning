#MODEL - 3 Summary on process of improving prediction accurary
#Batch Size - 64
#Epochs - 3
#Input max length - 20 
#Training = 75 percent
#Two  LSTMN Layer
#One drop out layer = 0.2
#Testing = 25 percent
#optimizer = "adam"
#Result - 84.94 percent
# set seed
set.seed(1337)

# setting addtional hyperparmeters
input_maxlen = 20
batch_size = 64
epochs = 3
dropout = 0.2

# Data preprocessing 

# Data tokenization
tokenizer <- text_tokenizer(num_words = 50000)
tokenizer %>% 
  fit_text_tokenizer(mydata$Headline)
headline <- texts_to_sequences(tokenizer, mydata$Headline)

# padding and chopping of  all input sequences to input_maxlen as setted above
headline_p <- pad_sequences(headline,maxlen = input_maxlen,value = 50000 + 1)

# testing and training split
sample <- sample.int(nrow(headline_p),size = 0.10*nrow(headline_p))

train_headline <- headline_p[-sample,]
train_sar <- mydata$is_sarcastic[-sample]

test_headline <- headline_p[sample,]
test_sar <- mydata$is_sarcastic[sample]


# Configuring input layer
input <- layer_input(shape = c(input_maxlen), name = "input_headline")

# Configuring  word embedding layer
word_embedder <- layer_embedding( 
  input_dim = 50000 + 2,
  output_dim = 128,     
  input_length = input_maxlen,    
  embeddings_regularizer = regularizer_l2(0.0001) 
)

# Configuring 1st lstm sequence embedder
seq_embedder <- layer_lstm(
  units = 128,
  return_sequences = TRUE,
  kernel_regularizer = regularizer_l2(0.0001)  
)

# Configuring 2nd lstm sequence embedder
seq_embedder2 <- layer_lstm(
  units = 128,
  kernel_regularizer = regularizer_l2(0.0001)  
)

# devevloping the final output layer by specifying complete structure as below
output <- input %>% 
  word_embedder() %>% 
  seq_embedder() %>% 
  layer_dropout(dropout) %>%
  seq_embedder2() %>%
  layer_dense(units = 1, activation = "sigmoid")

# configuring the model specificiation
model <- keras_model(input,output)
model %>% compile(
  optimizer = "adam", 
  metrics = c('accuracy'), 
  loss = "binary_crossentropy"
) #RMSProp, AdaGrad, sgd, Adadelta  #adam

# Confirming the model details
summary(model)

#Evaluation process starts
# Fit model to data
history <- model %>% fit(
  train_headline,
  train_sar,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.2
)

plot(history)


score <- model %>% evaluate(
  test_headline,
  test_sar,
  verbose = 0
)


score

