
from __future__ import division
import numpy as np
import vmo.analysis as van
import vmo.VMO.utility as utl
import sklearn.preprocessing as pre
import scipy.stats as stats
import os, itertools, csv, librosa, vmo, glob, functools

file_path = '../JKUPDD-Oct2013/groundTruth/'
song_list = ['bachBWV889Fg', 'beethovenOp2No1Mvt3', 'chopinOp24No4', 'gibbonsSilverSwan1612', 'mozartK282Mvt2']
perf_list = ['gould','gulda','rubinstein','collins','gulda']
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

def beat_time_align(beat_num, midi_frame):
    midi_beat = np.arange(midi_frame[0],midi_frame[1]+0.125,0.125)
    audio_beat = np.arange(beat_num+1)
    midi_num = midi_frame[1] - midi_frame[0] + 1
    audio_beat_scaled = audio_beat*(midi_num/(beat_num)) + midi_frame[0]
    aligned_beat = [midi_beat[np.argmin(abs(beat - midi_beat))] for beat in audio_beat_scaled]
    return aligned_beat

sr = 11025
fft_size = 8192
hop_size = 64

r = (0.0, 2.0, 0.01)
shift = 5 # Current optimal setting
step = 3 # Current optimal setting
# step = 1
trnspose_inv = functools.partial(utl.transpose_inv, shift=shift, step=step) # Current optimal setting

audio_list = [{'audio':[file_path+song+'/polyphonic/perfAud/'+perf+'/audio/'+a for a in os.listdir(file_path+song+'/polyphonic/perfAud/'+perf+'/audio/') if a.endswith('wav')][0],
               'info':(song, bpm, beat_range),
               'pattern':get_occurence(file_path+song+'/polyphonic/deadSym/repeatedPatterns/', pattern_list)}
                for song, perf, bpm, beat_range in zip(song_list, perf_list, bpm_list, brange)]

for ind in range(5):
    audio_test = audio_list[ind]
    print "start"+ audio_test['audio']

    y, sr = librosa.load(audio_test['audio'], sr = sr)
    C = librosa.feature.chromagram(y=y, sr=sr, n_fft=fft_size, hop_length=hop_size)
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, hop_length=hop_size)

    subbeats = []
    for bs, be in zip(beats[:-1],beats[1:]):
        subbeats.extend(np.linspace(bs, be, num = 2, endpoint = False).astype('int').tolist())
    subbeats.extend(np.linspace(beats[-1], C.shape[1], num = 2, endpoint = False).astype('int').tolist())
    subbeats.append(C.shape[1])
    C_sync = librosa.feature.sync(C, subbeats, aggregate=np.median)
    feature = np.log(C_sync+np.finfo(float).eps)
    feature = pre.normalize(feature, axis = 0)

    feature = librosa.feature.stack_memory(feature, n_steps=3, delay=1)

    chroma_frames = feature.transpose()
    ideal_v_inv, _ir = vmo.find_threshold(chroma_frames, r = r, flag = 'a', dfunc = 'other',
                                          dfunc_handle = trnspose_inv, dim=chroma_frames.shape[1])
    oracle_inv= vmo.build_oracle(chroma_frames, flag = 'a',
                                 threshold = ideal_v_inv[1],
                                 feature = 'chroma', dfunc = 'other',
                                 dfunc_handle = trnspose_inv, dim=chroma_frames.shape[1])

    min_len = int(np.mean(oracle_inv.lrs)/2)  # Current optimal setting
    # min_len =5
    pattern = van.find_repeated_patterns(oracle_inv, lower=min_len)
    beat_aligned = beat_time_align(len(subbeats), brange[ind])

    pattern_aligned = []
    for pttr in pattern:
        pttr_aligned = []
        for p in pttr[0]:
            p_start = beat_aligned[p-pttr[1]-1]
            p_end = beat_aligned[p-1]
            pttr_aligned.append([p_start, p_end])
        pttr_aligned = sorted(pttr_aligned, key=lambda pt: pt[0])
        pattern_aligned.append(pttr_aligned)


    csv_path = file_path+song_list[ind]+'/polyphonic/deadSym/csv/'
    csv_file_path = glob.glob(csv_path+'*')
    csv_file = open(csv_file_path[0], 'r')
    cs = list(csv.reader(csv_file))
    cs_float = [[float(_c) for _c in c[:2]] for c in cs]
    csv_file.close()

    found = open('../../../MATLAB/themeDiscovery/vmoOut/'+song_list[ind]+'-polyphonic.txt','w')

    for i, pattern in enumerate(pattern_aligned):
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
    print "end"+ audio_test['audio']
