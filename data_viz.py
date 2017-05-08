#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
viz for lonsea project (transitioning to python)

part 1: job rank on gender

"""
import os, math
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
fpath = '/home/kln/Documents/proj/lonsea'
os.chdir(fpath)
main_u_df = pd.read_csv(fpath+'/main_u.csv')

##### part 1
### job rank on gender
# dist job rank
# sns.set_style("darkgrid", {"axes.facecolor": ".9"})# dark grid
def univar(x,var,title):
    sns.set(style="ticks",font_scale=2)# remove grid
    sns.set_style("white", {"font.family": "serif","font.serif": ["Times", "Palatino", "serif"]})
    ax = sns.distplot(x, bins = 10, rug=True, rug_kws={"color": "r"}, kde_kws={"color": "k", "lw": 3, "label": "Kernel Density Estimation"}, color = 'black')
    ax.set(xlabel=var)
    ax.axes.set_title(title, fontsize=36,color="r",alpha=0.9)
    return ax
x = main_u_df.loc[:,'avg_rank']
plt.figure(figsize=(24, 12))
ax = univar(x,'Average Job Rank','Job Rank in LONSEA DB')    
ax.set_xlim(-.9,9.9)
fig = ax.get_figure()
fig.savefig('figures/job_rank_mean.png')
# count plot
x = [math.floor(e) for e in main_u_df.loc[:,'avg_rank']]
plt.figure(figsize=(24, 12))
sns.set(style="ticks",font_scale=2)# remove grid
sns.set_style("white", {"font.family": "serif","font.serif": ["Times", "Palatino", "serif"]})
ax = sns.countplot(x, color = 'k')
ax.set(xlabel='Average Job Rank')
ax.set(ylabel='Count')
ax.axes.set_title('Job Rank in LONSEA DB', fontsize=36,color="r",alpha=0.9)
fig = ax.get_figure()
fig.savefig('figures/job_rank_count.png')
# dist of job rank on gender
idx1 = main_u_df.loc[:,'gender'] == 'male'
idx2 = main_u_df.loc[:,'gender'] == 'female'                
x1 = main_u_df.loc[idx1,'avg_rank']
x2 = main_u_df.loc[idx2,'avg_rank']
f, (ax1, ax2) = plt.subplots(ncols=2,figsize=(24,12))
f1 = sns.distplot(x1, ax=ax1, bins = 10, rug=True, rug_kws={"color": "r"}, kde_kws={"color": "k", "lw": 3, "label": "KDE"}, color = 'black')
f1.set(xlabel='Job Rank')
f1.axes.set_title('Male', fontsize=36,color="r",alpha=0.9)
f2 = sns.distplot(x2, ax=ax2, bins = 10, rug=True, rug_kws={"color": "r"}, kde_kws={"color": "k", "lw": 3, "label": "KDE"}, color = 'black')
f2.set(xlabel='Job Rank')
f2.axes.set_title('Female', fontsize=36,color="r",alpha=0.9)
f.savefig('figures/job_rank_gender.png')
# boxplot of job rank on gender
plt.figure(figsize=(12,12))
ax = sns.boxplot(x = 'gender', y = 'avg_rank', data = main_u_df, color = 'r')
ax.set(xlabel='Gender',ylabel='Average Job Rank')
ax.axes.set_title('Gender Differences', fontsize=36,color="r",alpha=0.9)
fig = ax.get_figure()
fig.savefig('figures/job_rank_test.png')
