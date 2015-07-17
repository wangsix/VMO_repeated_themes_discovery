

## Discovery of repeated themes - Test with VMO
import numpy as np
import vmo.analysis as van
import vmo.draw as vdr
import vmo.VMO.utility as utl
import matplotlib.pyplot as plt
import sklearn.preprocessing as pre
import scipy.stats as stats
import os, itertools, csv, librosa, vmo, glob, functools, pickle


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

def cardinality_score(pttr1_end, pttr2_end, pttr1_len, pttr2_len):
    overlap = np.intersect1d(range(pttr1_end+1-pttr1_len,pttr1_end+1),range(pttr2_end+1-pttr2_len,pttr2_end+1))
    return float(overlap.shape[0])/np.float(np.max([pttr1_len, pttr2_len])), overlap

def score_matrix(pttr1, pttr2):
    s_mat = []
    o_mat = []
    for oc1 in pttr1[0]:
        s_row = []
        o_row = []
        for oc2 in pttr2[0]:
            s, over = cardinality_score(oc1, oc2, pttr1[1], pttr2[1])
            s_row.append(s)
            o_row.append(over)
        s_mat.append(s_row)
        o_mat.append(o_row)
    return s_mat, o_mat

def get_pttr(s_mat, o_mat, lower):
    s_mat = np.array(s_mat)
    o_mat = np.array(o_mat)
    if s_mat.any():
        s_row = np.sum(s_mat, axis = 1)
        s_mat = s_mat[s_row.nonzero()]
        o_mat = o_mat[s_row.nonzero()]
        i_row = np.argmax(s_mat, axis = 1)
        if np.unique(i_row).shape[0] == i_row.shape[0] and i_row.shape[0]>1:
            pttr = o_mat[range(o_mat.shape[0]),i_row]
            pttr_len = [r.shape[0] for r in pttr]
            pttr_end = [r[-1] for r in pttr]
            if np.min(pttr_len) > lower:
                return [pttr_end,np.min(pttr_len)]
            else:
                return []
        else:
            return []
    else:
        return []

### Parse Dataset

audio_list = [{'audio':[file_path+song+'/polyphonic/audio/'+a for a in os.listdir(file_path+song+'/polyphonic/audio/') if a.endswith('wav')][0],
               'info':(song, bpm, beat_range),
               'pattern':get_occurence(file_path+song+'/polyphonic/repeatedPatterns/', pattern_list)} 
                for song, bpm, beat_range in zip(song_list, bpm_list, brange)]

performance_vmo_path = '../../research/notebook/motif_discovery/motif_vmo_0421'
f = open(performance_vmo_path, 'r')
vmo_dict = pickle.load(f)
f.close()

feature_path = '../../research/notebook/motif_discovery/motif_feature_0420'
g = open(feature_path, 'r')
feature_dict = pickle.load(g)
g.close()

pattern_vec = {}
pattern_mat_vec = {}
ground_vec = []

sr = 11025
fft_size = 8192
hop_size = 128

for ind in range(5):
    print 'Begin processing '+ song_list[ind]
    audio_test = audio_list[ind]
    oracle_vec = vmo_dict[song_list[ind]]
    piece_dict = feature_dict[song_list[ind]]
    c_list = piece_dict['chroma']
    b_list = piece_dict['beats']
    query = c_list[0]
    q_b = b_list[0]
    
    subbeats = []
    for bs, be in zip(q_b[:-1],q_b[1:]):
        subbeats.extend(np.linspace(bs, be, num = 2, endpoint = False).astype('int').tolist())
    subbeats.extend(np.linspace(q_b[-1], query.shape[1], num = 2, endpoint = False).astype('int').tolist())
    subbeats.append(query.shape[1])
   
    ground = np.zeros((len(audio_test['pattern']), audio_test['info'][2][1]-audio_test['info'][2][0]))
    for i,p in enumerate(audio_test['pattern']):
        for _p in p:
            start = _p[0] - audio_test['info'][2][0]
            end = _p[1] - audio_test['info'][2][0]
            ground[i][start:end+1] = 1
    
    ground_vec.append(ground)
    
    pttr_vec = []
    min_len = int(np.mean(oracle_vec[0].lrs)/2)
    
    for oracle in oracle_vec:
        pttr_vec.append(van.find_repeated_patterns(oracle, lower = min_len))
#     pattern_vec[song_list[ind]] = pttr_vec
#     pattern_mat_vec[song_list[ind]] = pttr_mat_vec
# 
#     pttr_vec = pattern_vec[song_list[ind]]
#     pttr_mat_vec = pattern_mat_vec[song_list[ind]]
#     oracle_vec = vmo_dict[song_list[ind]]
    
    ref_pttr = pttr_vec[0][:]
    per_pttr = pttr_vec[1:5][:]
    add_pttr_list = []

    pttr = per_pttr.pop()
#     min_len = int(np.mean(oracle_vec[0].lrs)/2)
    while per_pttr != []:
        for i,p in enumerate(pttr):
            for qttr in per_pttr:            
                for j,q in enumerate(qttr):
                    s, o = score_matrix(p,q)
                    add_pttr = get_pttr(s,o,min_len)
                    if add_pttr != []:
                        add_pttr_list.append(add_pttr)    
        pttr = per_pttr.pop()

    pattern_plot = np.zeros((len(add_pttr_list), vmo_dict[song_list[ind]][0].n_states-1))
    for i,p in enumerate(add_pttr_list):
        length = p[1]
        for s in p[0]:
            pattern_plot[i][s-length:s-1] = 1
            
    pattern_dot = pattern_plot.dot(pattern_plot.T)
    add_pttr_list2 = []
    row_hist = []
    for pttr_dot in pattern_dot:
        row_ind = np.where(pttr_dot > 0)[0].tolist()
        if row_ind not in row_hist:
            row_hist.append(row_ind)
            local_pttr = [add_pttr_list[i] for i in row_ind]
            if local_pttr != []:
                p = local_pttr.pop()
                while local_pttr != [] and p != []:
                    q = local_pttr.pop()
                    s, o = score_matrix(p,q)
                    p = get_pttr(s,o,lower = min_len)
                if p != []:
                    add_pttr_list2.append(p)   

    pattern2 = pttr_vec[0]
    pattern2.extend(add_pttr_list2)
#     pattern_plot2 = np.zeros((len(pattern2), vmo_dict[song_list[ind]][0].n_states-1))
#     for i,p in enumerate(pattern2):
#         length = p[1]
#         for s in p[0]:
#             pattern_plot2[i][s-length:s-1] = 1

    pattern_time = []
    for pttr in pattern2:
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
        pt_bpm = sorted(pt_bpm, key = lambda pt:pt[0])
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




