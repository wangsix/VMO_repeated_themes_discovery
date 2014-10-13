
import numpy as np
import vmo.analysis as van
import matplotlib.pyplot as plt
import sklearn.preprocessing as pre
import os, itertools, csv, librosa, vmo, glob, time


file_path = '../JKUPDD-Aug2013/groundTruth/'
song_list = ['bachBWV889Fg', 'beethovenOp2No1Mvt3', 'chopinOp24No4', 'gibbonsSilverSwan1612', 'mozartK282Mvt2']
bpm_list = [84, 192, 138, 54, 120]
brange = [[1,110],[-1,555],[-1, 556],[1, 80],[-1,526]]
pattern_list = ['barlowAndMorgensternRevised', 'bruhn','schoenberg','sectionalRepetitions','tomCollins']

def get_occurence(f_path, pattern_list):
    f = itertools.chain.from_iterable([[_f+'/'+c+'/occurrences/csv/' for c in os.listdir(f_path+_f+'/') if not c.startswith('.')] 
                                        for _f in os.listdir(f_path) if (not _f.startswith('.') and _f in pattern_list)])
    f = list(f)
    occur = [[_c for _c in os.listdir(f_path+'/'+_f) if not _c.startswith('.')] for _f in f[:]]
    out = []
    for c,s in zip(occur, f):
        occ_list = []
        for _c in c:
            csv_file = open(f_path+s+_c,'r')
            cs = list(csv.reader(csv_file))
            region = [float(cs[0][0]),float(cs[-1][0])]
            occ_list.append(region)
        out.append(occ_list)
    return out

def array_rotate(a):
    _a = a
    for _i in range(1,a.size):
        _a = np.vstack((_a, np.roll(a,_i)))
    return _a
        
def trnspose_inv(a, b_vec):
    d_vec = []
    a = np.array(a)
    a_mat = array_rotate(a)
    for b in b_vec:
        d = a_mat - np.array(b)
        d = np.sqrt((d*d).sum(axis=1))
        d_vec.append(d.min())
    return np.array(d_vec)

audio_list = [{'audio':[file_path+song+'/polyphonic/audio/'+a for a in os.listdir(file_path+song+'/polyphonic/audio/') if a.endswith('wav')][0],
               'info':(song, bpm, beat_range),
               'pattern':get_occurence(file_path+song+'/polyphonic/repeatedPatterns/', pattern_list)} 
                for song, bpm, beat_range in zip(song_list, bpm_list, brange)]

fft_size = 8192
hop_size = 64
sr = 11025
feature_mat = []
subbeat_mat = []
beat_mat = []
for ind in range(5):
    audio_test = audio_list[ind]
    y, sr = librosa.load(audio_test['audio'], sr = sr)
    C = librosa.feature.chromagram(y=y, sr=sr, n_fft=fft_size, hop_length=hop_size)
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, hop_length=hop_size)  
    subbeats = []
    for bs, be in zip(beats[:-1],beats[1:]):
        subbeats.extend(np.linspace(bs, be, num = 2, endpoint = False).astype('int').tolist())
    subbeats.extend(np.linspace(beats[-1], C.shape[1], num = 2, endpoint = False).astype('int').tolist())
    C_sync = librosa.feature.sync(C, subbeats, aggregate=np.median)
    feature = np.log(C_sync+np.finfo(float).eps)
    feature = pre.normalize(feature)
    feature_mat.append(feature)
    beat_mat.append(beats)
    subbeat_mat.append(subbeats)
    
min_len_list = []
for ind in range(5):
    audio_test = audio_list[ind]    
    ground = np.zeros((len(audio_test['pattern']), audio_test['info'][2][1]-audio_test['info'][2][0]))
    len_list = []
    for i,p in enumerate(audio_test['pattern']):
        for _p in p:
            start = _p[0] - audio_test['info'][2][0]
            end = _p[1] - audio_test['info'][2][0]
            len_list.append(end-start)
            ground[i][start:end+1] = i+1
    min_len = int(min(len_list)*len(subbeat_mat[ind])/(audio_test['info'][2][1]-audio_test['info'][2][0]))+1
    min_len_list.append(min_len)
print min_len_list

start_time = time.time()
for ind in range(5):
    chroma_frames = feature_mat[ind].transpose()
    r = (0.0, 1.0, 0.01) 
    ideal_v_inv = vmo.find_threshold(chroma_frames, r = r,flag = 'a', dfunc = 'other', dfunc_handle = trnspose_inv, VERBOSE = False)
    oracle_inv= vmo.build_oracle(chroma_frames, flag = 'a', 
                                threshold = ideal_v_inv[0][1], 
                                feature = 'chroma', dfunc = 'other', dfunc_handle = trnspose_inv)
    pattern = van.find_repeated_patterns(oracle_inv, lower = 5)
print str(time.time()-start_time)




