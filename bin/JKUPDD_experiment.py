

# Discovery of repeated themes - Test with VMO
import numpy as np
import vmo.analysis as van
import vmo.VMO.utility as utl
import sklearn.preprocessing as pre
import os, itertools, csv, librosa, vmo, glob, functools

# Load JKUPDD dataset

file_path = '../JKUPDD-Aug2013/groundTruth/'
song_list = ['bachBWV889Fg', 'beethovenOp2No1Mvt3', 'chopinOp24No4', 'gibbonsSilverSwan1612', 'mozartK282Mvt2']
bpm_list = [84, 192, 138, 54, 120]
brange = [[1,110],[-1,555],[-1, 556],[1, 80],[-1,526]]
pattern_list = ['barlowAndMorgensternRevised', 'bruhn','schoenberg','sectionalRepetitions','tomCollins']

# Define Dataset Parser & Transpose Invariant Distance Function


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
            region = [float(cs[0][0]), float(cs[-1][0])]
            occ_list.append(region)
        out.append(occ_list)
    return out

# Parse Dataset

audio_list = [{'audio':[file_path+song+'/polyphonic/audio/'+a for a in os.listdir(file_path+song+'/polyphonic/audio/') if a.endswith('wav')][0],
               'info':(song, bpm, beat_range),
               'pattern':get_occurence(file_path+song+'/polyphonic/repeatedPatterns/', pattern_list)}
                for song, bpm, beat_range in zip(song_list, bpm_list, brange)]
sr = 11025
fft_size = 8192
hop_size = 64
# hop_size = 128
feature_mat = []

r = (0.0, 1.5, 0.01) 
shift = 5 # Current optimal setting
step = 3 # Current optimal setting
# step = 1
trnspose_inv = functools.partial(utl.transpose_inv, shift=shift, step=step) # Current optimal setting
for ind in range(5):

    audio_test = audio_list[ind]
        
    y, sr = librosa.load(audio_test['audio'], sr = sr)
#     fft_size = 8192
#     hop_size = 128
    C = librosa.feature.chroma_cqt(y=y, sr=sr, hop_length=hop_size)
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, hop_length=hop_size)
    
    ### Sub-Beat-Synchronous Chromagram
    subbeats = []
    for bs, be in zip(beats[:-1],beats[1:]):
        subbeats.extend(np.linspace(bs, be, num=2, endpoint = False).astype('int').tolist())
    subbeats.extend(np.linspace(beats[-1], C.shape[1], num=2, endpoint=False).astype('int').tolist())
    C_sync = librosa.util.sync(C, subbeats, aggregate=np.median)
    subbeats.append(C.shape[1])
    feature = np.log(C_sync+np.finfo(float).eps)
    feature = pre.normalize(feature, axis=0)
    
    ### Create VMO     
    chroma_frames = feature.transpose()
    ideal_v_inv = vmo.find_threshold(chroma_frames, r=r, flag='a',
                                     dfunc='other', dfunc_handle=trnspose_inv, dim=chroma_frames.shape[1])
    oracle_inv= vmo.build_oracle(chroma_frames, flag='a',
                                threshold=ideal_v_inv[0][1],
                                feature='chroma', dfunc='other', dfunc_handle=trnspose_inv,
                                dim=chroma_frames.shape[1])

    ### Gather Ground Truth from Dataset
        
    ground = np.zeros((len(audio_test['pattern']), audio_test['info'][2][1]-audio_test['info'][2][0]))
    len_list = []
    for i,p in enumerate(audio_test['pattern']):
        for _p in p:
            start = _p[0] - audio_test['info'][2][0]
            end = _p[1] - audio_test['info'][2][0]
            len_list.append(end-start)
            ground[i][start:end+1] = i+1
#     min_len = int(min(len_list)*len(subbeats)/(audio_test['info'][2][1]-audio_test['info'][2][0])/2)+1
#     min_len = int(stats.hmean(np.array(oracle_inv.lrs)[np.where(np.array(oracle_inv.lrs) != 0)]))
#     min_len = 5
    min_len = int(np.mean(oracle_inv.lrs)/2) # Current optimal setting

    ### Extract Repeated Suffixes from VMO
    
    pattern = van.find_repeated_patterns(oracle_inv, lower=min_len)

    ### Beat Time to Time 
     
    pattern_time = []
    for pttr in pattern:
        pttr_time = []
        for p in pttr[0]:
            time = librosa.core.frames_to_time(np.array([subbeats[p-pttr[1]],subbeats[p-1]]),
                                                sr=sr, n_fft=fft_size, hop_length=hop_size)
            pttr_time.append(time)
        pattern_time.append(pttr_time)
     
    pattern_bpm = []
    for pt_time in pattern_time:
        pt_bpm = []
        for p in pt_time:
            p_b_start = np.floor((p[0]/60.0)*audio_test['info'][1])
            p_b_end = np.ceil((p[1]/60.0)*audio_test['info'][1])
            pt_bpm.append([p_b_start+audio_test['info'][2][0], p_b_end+audio_test['info'][2][0]])
        pt_bpm = sorted(pt_bpm, key=lambda pt: pt[0])
        pattern_bpm.append(pt_bpm)

    csv_path = file_path+song_list[ind]+'/polyphonic/csv/'
    csv_file_path = glob.glob(csv_path+'*')
    csv_file = open(csv_file_path[0], 'r')
    cs = list(csv.reader(csv_file))
    cs_float = [[float(_c) for _c in c[:2]] for c in cs]
    csv_file.close()
         
    found = open('../../../MATLAB/themeDiscovery/vmoOut/'+song_list[ind]+'-polyphonic.txt','w')
     
    for i, pattern in enumerate(pattern_bpm):
        found.write('pattern'+str(i+1)+'\n')
        for k,p in enumerate(pattern):
            found.write('occurrence'+str(k+1)+'\n')
            start = np.argmin([abs(p[0]-c[0]) for c in cs_float])
            ending = np.argmin([abs(p[1]-c[0]) for c in cs_float])
            while ending != len(cs_float)-1 and cs_float[ending][0] == cs_float[ending+1][0]:
                ending += 1
            for j in range(start, ending+1):
                s = str(cs_float[j][0])+', '+str(cs_float[j][1])+'\n'
                found.write(s)
    found.close()



