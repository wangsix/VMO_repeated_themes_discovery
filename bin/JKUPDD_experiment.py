

## Discovery of repeated themes - Test with VMO
import numpy as np
import vmo.analysis as van
import matplotlib.pyplot as plt
import sklearn.preprocessing as pre
import os, itertools, csv, librosa, vmo, glob


### Load JKUPDD dataset

file_path = '../JKUPDD-Aug2013/groundTruth/'
song_list = ['bachBWV889Fg', 'beethovenOp2No1Mvt3', 'chopinOp24No4', 'gibbonsSilverSwan1612', 'mozartK282Mvt2']
bpm_list = [84, 192, 138, 54, 120]
brange = [[1,110],[-1,555],[-1, 556],[1, 80],[-1,526]]
pattern_list = ['barlowAndMorgensternRevised', 'bruhn','schoenberg','sectionalRepetitions','tomCollins']

### Define Dataset Parser & Transpose Invariant Distance Function 

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


### Parse Dataset

audio_list = [{'audio':[file_path+song+'/polyphonic/audio/'+a for a in os.listdir(file_path+song+'/polyphonic/audio/') if a.endswith('wav')][0],
               'info':(song, bpm, beat_range),
               'pattern':get_occurence(file_path+song+'/polyphonic/repeatedPatterns/', pattern_list)} 
                for song, bpm, beat_range in zip(song_list, bpm_list, brange)]
sr = 11025
fft_size = 8192
hop_size = 64
feature_mat = []
for ind in range(5):
    audio_test = audio_list[ind]
    y, sr = librosa.load(audio_test['audio'], sr = sr)
    C = librosa.feature.chromagram(y=y, sr=sr, n_fft=fft_size, hop_length=hop_size)
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, hop_length=hop_size)
    
    ### Sub-Beat-Synchronous Chromagram
    subbeats = []
    for bs, be in zip(beats[:-1],beats[1:]):
        subbeats.extend(np.linspace(bs, be, num = 2, endpoint = False).astype('int').tolist())
    subbeats.extend(np.linspace(beats[-1], C.shape[1], num = 2, endpoint = False).astype('int').tolist())
    subbeats.append(C.shape[1])
    C_sync = librosa.feature.sync(C, subbeats, aggregate=np.median)
    feature = np.log(C_sync+np.finfo(float).eps)
    feature = pre.normalize(feature)
    feature_mat.append(feature)
### Chromagram and Beat Extraction

for ind in range(5):

    audio_test = audio_list[ind]
        
    y, sr = librosa.load(audio_test['audio'], sr = 11025)
    fft_size = 8192
    hop_size = 64
    C = librosa.feature.chromagram(y=y, sr=sr, n_fft=fft_size, hop_length=hop_size)
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, hop_length=hop_size)
    
    ### Sub-Beat-Synchronous Chromagram
    subbeats = []
    for bs, be in zip(beats[:-1],beats[1:]):
        subbeats.extend(np.linspace(bs, be, num = 2, endpoint = False).astype('int').tolist())
    subbeats.extend(np.linspace(beats[-1], C.shape[1], num = 2, endpoint = False).astype('int').tolist())
    C_sync = librosa.feature.sync(C, subbeats, aggregate=np.median)
    feature = np.log(C_sync+np.finfo(float).eps)
    feature = pre.normalize(feature)
    
    ### Create VMO     
    chroma_frames = feature.transpose()
    r = (0.0, 1.5, 0.01) 
    ideal_v_inv = vmo.find_threshold(chroma_frames, r = r,flag = 'a', dfunc = 'other', dfunc_handle = trnspose_inv, VERBOSE = False)
    oracle_inv= vmo.build_oracle(chroma_frames, flag = 'a', 
                                threshold = ideal_v_inv[0][1], 
                                feature = 'chroma', dfunc = 'other', dfunc_handle = trnspose_inv)
    
    
    ### Create Recurrence Plot with VMO
        
    selfmat = van.create_selfsim(oracle_inv, method = 'rsfx')
    
    ### Gather Ground Truth from Dataset
        
    ground = np.zeros((len(audio_test['pattern']), audio_test['info'][2][1]-audio_test['info'][2][0]))
    len_list = []
    for i,p in enumerate(audio_test['pattern']):
        for _p in p:
            start = _p[0] - audio_test['info'][2][0]
            end = _p[1] - audio_test['info'][2][0]
            len_list.append(end-start)
            ground[i][start:end+1] = i+1
    min_len = int(min(len_list)*len(subbeats)/(audio_test['info'][2][1]-audio_test['info'][2][0])/2)+1
    
    ### Plots of Chromagram, Sub-Beat-Synchronous Chromagram, Recurrence Plot and Ground Truth of Repeated Themes
        
#     plt.figure(figsize=(8,14))
#     
#     plt.subplot2grid((7,1),(0,0))
#     librosa.display.specshow(C, y_axis='chroma', sr=sr, hop_length=hop_size, x_axis='time', vmin=0, vmax=1)
#     plt.title('Chromagram')
#     
#     plt.subplot2grid((7,1),(1,0))
#     librosa.display.specshow(feature, y_axis='chroma', x_axis='frames')
#     plt.title('Beat-synchronous Chromagram')
#     
#     plt.subplot2grid((7,1),(2,0), rowspan = 4)
#     plt.imshow(selfmat, cmap = 'gray', interpolation = 'nearest')
#     
#     plt.subplot2grid((7,1),(6,0))
#     plt.imshow(ground, interpolation = 'nearest', aspect = 'auto')
#     plt.title('Ground Truth of Repeated Themes Discovery - '+ audio_test['info'][0])
#     plt.yticks([])
#     
#     plt.tight_layout()
    
    ### Check IR Profile
        
#     x_v = [i[1] for i in ideal_v_inv[1]]
#     y_v = [i[0] for i in ideal_v_inv[1]]
#     plt.figure()
#     plt.plot(x_v, y_v, linewidth = 2)
#     plt.title('IR vs. Threshold Value(vmo)', fontsize = 18)
#     plt.grid(b = 'on')
#     plt.xlabel('Threshold', fontsize = 14)
#     plt.ylabel('IR', fontsize = 14)
#     plt.xlim(0.0,1.5)
#     plt.tight_layout()
    
    ### Extract Repeated Suffixes from VMO
        
    pattern = van.find_repeated_patterns(oracle_inv, lower = min_len)
#     pattern_mat = np.zeros((len(pattern), oracle_inv.n_states-1))
#     for i,p in enumerate(pattern):
#         length = p[1]
#         for s in p[0]:
#             pattern_mat[i][s-length:s-1] = i+5
# 
#     ### Plot Ground Truth and Found Repeated Suffixes
#         
#     plt.figure(figsize = (12,8))
#     plt.subplot(211)
#     plt.imshow(ground, interpolation = 'nearest', aspect = 'auto')
#     plt.title('Ground Truth of Repeated Themes Discovery - '+ audio_test['info'][0])
#     plt.yticks([])
#     plt.subplot(212)
#     plt.imshow(pattern_mat, interpolation = 'nearest', aspect = 'auto')
#     plt.title('Found Pattern with VMO - '+ audio_test['info'][0])
#     plt.yticks([])
#     plt.tight_layout()
#     
#     
#     ### Beat Time to Time 
#     
#     pattern_time = []
#     for pttr in pattern:
#         pttr_time = []
#         for p in pttr[0]:
#             time = librosa.core.frames_to_time(np.array([subbeats[p-pttr[1]+1],subbeats[p]]), 
#                                                 sr=sr, n_fft=fft_size, hop_length=hop_size)
#             pttr_time.append(time)
#         pattern_time.append(pttr_time)
#     
#     pattern_bpm = []
#     for pt_time in pattern_time:
#         pt_bpm = []
#         for p in pt_time:
#             p_b_start = np.floor((p[0]/60.0)*audio_test['info'][1])
#             p_b_end = np.ceil((p[1]/60.0)*audio_test['info'][1])
#             pt_bpm.append([p_b_start+audio_test['info'][2][0], p_b_end+audio_test['info'][2][0]])
#         pt_bpm = sorted(pt_bpm, key = lambda pt:pt[0])
#         pattern_bpm.append(pt_bpm)
#     
#     
#     csv_path = file_path+song_list[ind]+'/polyphonic/csv/'
#     csv_file_path = glob.glob(csv_path+'*')
#     csv_file = open(csv_file_path[0], 'r')
#     cs = list(csv.reader(csv_file))
#     cs_float = [[float(_c) for _c in c[:2]] for c in cs]
#     csv_file.close()
#         
#     found = open('/Users/Wang/Documents/MATLAB/themeDiscovery/vmoOut/'+song_list[ind]+'.txt','w')
#     
#     for i, pattern in enumerate(pattern_bpm):
#         found.write('pattern'+str(i+1)+'\n')
#         for k,p in enumerate(pattern):
#             found.write('occurrence'+str(k+1)+'\n')
#             start = np.argmin([abs(p[0]-c[0]) for c in cs_float])
#             ending = np.argmin([abs(p[1]-c[0]) for c in cs_float])
#             while ending != len(cs_float)-1 and cs_float[ending][0] == cs_float[ending+1][0]:
#                 ending += 1
#             for j in range(start, ending+1):
#                 s = str(cs_float[j][0])+', '+str(cs_float[j][1])+'\n'
#                 found.write(s)
#     found.close() 



