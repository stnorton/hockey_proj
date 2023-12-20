#imports
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
from tensorflow.keras.layers import Activation, concatenate, Dense, Dropout, Embedding, Input, Reshape, Flatten
from tensorflow.keras.models import Model
from tensorflow.keras.utils import plot_model
from tensorflow.keras.preprocessing.text import Tokenizer
import os

#get filename as command line argument
filename = sys.argv[0]
year_name = sys.argv[1]

#change to correct directory
data_path = 'C:/Users/Sean/Documents/Projects/hockey_proj/data/'
os.chdir(data_path)


#read in data from csv
data_file = data_path + filename
all_df = pd.read_csv(data_file)

#we want to keep shots on net and goals
shots_df = all_df[all_df.event_type.isin(['SHOT', 'GOAL'])]

#keep only useful columns 
shots_df = shots_df[['event_type', 'secondary_type', 'event_player_1_name', 'strength_code', 'x_fixed', 'y_fixed', 'shot_distance', 'shot_angle', 'xg']]

#rename column and generate unique player_ids
shots_df = shots_df.rename(columns={'event_player_1_name': 'name'})
shots_df['pid'] = pd.factorize(shots_df['name'])[0]

#get key mapping pids to names
#get name index pairs from shots_df
unique_pairs = shots_df.drop_duplicates(subset=['name', 'pid'])
player_pid_mapping = unique_pairs.groupby('name')['pid'].unique().reset_index()
player_pid_mapping = pd.DataFrame(player_pid_mapping)
#convert pid to int
player_pid_mapping['pid'] = player_pid_mapping['pid'].astype(int)

#get dummies for shot type and strength_code
shots_df = pd.get_dummies(shots_df, columns = ['secondary_type', 'strength_code'], dtype=int)

#create goal_variable
shots_df['goal'] = np.where(shots_df['event_type'] == 'GOAL', 1, 0)

#scale numeric data to 0/1
scaler = MinMaxScaler()
num_cols = ['x_fixed', 'y_fixed', 'shot_distance', 'shot_angle', 'xg']
shots_df[num_cols] = scaler.fit_transform(shots_df[num_cols])

#convert player ids to a string 
shots_df.pid = shots_df.pid.astype('str')

#set up and run tokenizer
shooter_tokenizer = Tokenizer()

shooter_tokenizer.fit_on_texts(shots_df.pid)

#convert to sequences to feed neural net - all sequences are same length, no padding
shooters = shooter_tokenizer.texts_to_sequences(shots_df.pid)
shooters = [x[0] for x in shooters]
shooters = np.array(shooters)

#set target for fitting embeddings - goals
goal = np.array(shots_df.goal)
shot_info = shots_df.drop(columns=['pid', 'goal', 'name', 'event_type'], axis=1, inplace=False)
SHOT_COLS = shot_info.shape[1]
NUM_SHOOTERS = len(np.unique(shooters)) + 2
VEC_SIZE = 50

# the input layers
shooter_input = Input(shape=(1, ), name="shooter_input")
shot_input = Input(shape=(SHOT_COLS, ), name="shot_input")

# shooter layers
s1 = Embedding(NUM_SHOOTERS, VEC_SIZE, input_length=1)(shooter_input)
s2 = Flatten()(s1)
s3 = Dense(1, activation="sigmoid")(s2)

# shot layers
s4 = Dense(16, activation='relu')(shot_input)
s5 = Dense(8, activation='relu')(s4)

# concatenate both paths
concatenated = concatenate([s3, s5])

# output layer
output = Dense(1, activation='sigmoid')(concatenated)

# create model
model = Model(inputs=[shooter_input, shot_input], outputs=output)

#compile model
model.compile(optimizer="adam", loss = "binary_crossentropy", metrics = ['accuracy'])

#run model
X = [shooters, shot_info]
history = model.fit(X, goal, epochs=10)

#get embeddings
embedding_layer = model.get_layer('embedding')
shooter_embeddings = embedding_layer.get_weights()[0]

# make the embed vectors a pandas dataframe
shooter_embeddings = pd.DataFrame(shooter_embeddings)
shooter_embeddings.head()

# make the embed vectors a pandas dataframe
shooter_embeddings = pd.DataFrame(shooter_embeddings)
shooter_embeddings.head()

# name the columns
shooter_embeddings.columns = ["e" + str(i + 1) for i in range(shooter_embeddings.shape[1])]

# align the data by index
shooter_embeddings = pd.merge(shooter_embeddings, shooter_df, how='inner', left_index=True, right_index=True)

# clean up the index so its the player
shooter_embeddings.index = shooter_embeddings.playerid

#rename column
shooter_embeddings = shooter_embeddings.rename(columns = {'playerid':'pid'})

#merge names back in
shooter_embeddings['pid'] = shooter_embeddings.pid.astype(int)
shooter_embeddings = pd.merge(shooter_embeddings, player_pid_mapping, on='pid')

#generate out files
embed_out = data_path + 'shooter_embed_full' + year_name + '.csv'
shooter_embeddings.to_csv(embed_out)

names_out = data_path + 'embed_names_clean' + year_name + '.csv'