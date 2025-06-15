#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 10 13:44:46 2025

@author: kanep
"""

import matplotlib.pyplot as plt
import numpy as np

names =['Acts_young_for_age', 'Drinks_alcohol', 'Argues', 'Fails_to_finish_things', 'little_enjoy',
        'Bragging_Boasting', 'Cant_concentrate', 'Cant_get_their_mind_off_thoughts_obsessions',
        'restless_hyperactive', 'Clings_to_Adults', 'Complains_of_lonliness', 'Confused',
        'Cries', 'Bullying', 'Daydreams', 'Deliberately_harms_attempts_suicide', 'Demands_attention',
        'Destroys_own_things', 'Destroys_others_things', 'Disobedient_at_home', 'Disobedient_at_school',
        'Doesnt_get_along_with_kids', 'No_guilt_after_misbehaving', 'Easily_Jealous', 'Breaks_rules',
        'Fears_animals_situations_places', 'Fears_school', 'Fears_doing_bad', 'Feels_need_to_be_perfect',
        'Feels_no_one_loves_them', 'Feels_others_are_out_to_get_them', 'Feels_worthless_inferior',
        'Gets_hurt_lots', 'Gets_in_fights', 'Teased_lots', 'Hangs_around_others_who_get_in_trouble',
        'Hears_sounds_voices', 'Impulsive', 'Rather_be_alone', 'Lying_Cheating', 'Nervous_highstrung',
        'Nervous_movements_Twitching', 'Nightmares', 'Not_liked_by_other_kids', 'Constipated', 'fearful_anxious',
        'dizzy_lightheaded', 'Feels_guilty', 'Overtired', 'Aches_Pains', 'Headaches', 'Nausea',
        'Problems_with_Eyes', 'Rashes', 'Stomachaches', 'Vomiting', 'attacks_people', 'Picks_parts_of_body',
        'Plays_with_sex_parts_in_public', 'Plays_with_sex_parts_too_much', 'Poor_school_work', 
        'Clumsy', 'Prefers_being_with_older_kids', 'Prefers_being_with_younger_kids', 'Refuses_to_talk',
        'Repeats_acts_over_and_over', 'Runs_away_from_home', 'Screams', 'Secretive', 'Sees_things',
        'Self_conscious', 'Sets_fires', 'Sexual_problems', 'shy_timid', 'Sleeps_less_than_average',
        'Innatentive_easily_distracted', 'Speech_problem', 'stares_blankly', 'Steals_at_home', 'Steals_outside_the_home',
        'Stores_things_they_dont_need', 'Strange_behaviour', 'Strange_ideas', 'Stubborn_Sullen_Irratabile',
        'Sudden_changes_in_mood', 'Sulks_a_lot', 'Suspicious', 'Swearing', 'Talks_about_killing_self',
        'Talks_walks_in_sleep', 'Talks_too_much', 'Teases', 'Temper_tantrums', 'Thinks_about_sex_too_much',
        'Threatens_people', 'Thumb_sucking', 'Trouble_sleeping', 'Truancy', 'Underactive_slow_moving',
        'Unhappy_sad_depressed', 'loud', 'Vandalism', 'Whining', 'Withdrawn', 'Worries'] 
	
subc_names = ['ATT', 'RB', 'AGG', 'ATT', 'WD', 'ATT', 'ATT', 'TH', 'ATT', 'SOC', 'SOC', 'ATT', 'AD',
              'AGG', 'ATT', 'TH', 'ATT', 'AGG', 'AGG', 'AGG', 'AGG', 'SOC', 'RB', 'SOC', 'RB', 'AD',
              'AD', 'AD', 'AD', 'AD', 'SOC', 'AD', 'SOC', 'AGG', 'SOC', 'RB', 'TH', 'ATT', 'WD',
              'RB', 'AD', 'TH', 'SOM', 'SOC', 'SOM', 'AD','SOM', 'AD', 'SOM', 'SOM', 'SOM', 'SOM','SOM',
              'SOM','SOM','SOM', 'AGG', 'TH', 'TH', 'TH', 'ATT', 'SOC', 'RB', 'SOC', 'WD', 'TH', 'RB',
              'AGG', 'WD', 'TH', 'AD', 'RB', 'RB', 'WD', 'TH', 'ATT', 'SOC', 'ATT', 'RB', 'RB', 'TH',
              'TH', 'TH', 'AGG', 'AGG', 'AGG', 'AGG', 'RB', 'AD', 'TH', 'ATT', 'AGG', 'AGG', 'RB', 
              'AGG', 'RB', 'TH', 'RB', 'WD', 'WD', 'AGG', 'RB', 'ATT', 'AD', 'WD']
loadings = [0.58, 0.35, 0.72, 0.70, 0.63, 0.50, 0.80, 0.69, 0.71, 0.59, 0.65, 0.65, 0.60,
            0.68, 0.56, 0.62, 0.71, 0.77, 0.77, 0.76, 0.69, 0.73, 0.67, 0.65, 0.78, 0.46,
            0.57, 0.55, 0.38, 0.69, 0.69, 0.68, 0.49, 0.69, 0.64, 0.54, 0.57, 0.77, 0.51,
            0.67, 0.67, 0.58, 0.51, 0.72, 0.37, 0.63, 0.48, 0.58, 0.58, 0.38, 0.37, 0.48, 
            0.33, 0.30, 0.43, 0.34, 0.69, 0.47, 0.43, 0.43, 0.65, 0.60, 0.47, 0.51, 0.58, 
            0.67, 0.71, 0.71, 0.59, 0.59, 0.57, 0.50, 0.61, 0.41, 0.52, 0.81, 0.32, 0.65,
            0.68, 0.68, 0.51, 0.75, 0.65, 0.73, 0.76, 0.69, 0.71, 0.61, 0.63, 0.29, 0.55,
            0.63, 0.73, 0.60, 0.77, 0.16, 0.57, 0.59, 0.57, 0.71, 0.68, 0.73, 0.60, 0.64,
            0.59]

fig, ax = plt.subplots(figsize=(10, 40))
ax.barh(names, loadings, color="skyblue")
# Remove unnecessary spines
ax.tick_params(axis='y', labelsize=5.5)
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.set_xlabel("Standardised Loading", fontsize=12)
ax.set_ylabel("Item Names", fontsize=12)
ax.set_title("Bar Chart of Loadings by Item Names", fontsize=14)
plt.tight_layout()
plt.show()

# Get mean loading and AVE

mean_loading=np.mean(loadings)

squared_sum = sum(l**2 for l in loadings)
total_loadings = len(loadings)
AVE = squared_sum / total_loadings

