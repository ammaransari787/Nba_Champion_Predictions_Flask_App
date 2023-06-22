import nba_api
from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguegamefinder
from nba_api.stats.endpoints import commonallplayers
from nba_api.stats.endpoints import commonteamroster
from nba_api.stats.endpoints import playerawards
import pandas 
import numpy as np
import sys
import json
import time

if __name__ == '__main__':
    nba_teams = teams.get_teams()

    full_name_list = []

    for dict_ in nba_teams:
        if dict_['full_name'] != "Los Angeles Clippers":
            full_name_list.append(dict_['full_name'])
        else: 
            full_name_list.append('LA Clippers')

    num_all_star_dict_ = {}
    num_mvp_dict_ = {}
    num_all_defensive_dict_ = {}
    num_dpoy_dict_ = {}

    seasons_list = [2017,2018,2019,2020,2021,2022]

    for this_season in seasons_list:
        num_all_star_dict_[this_season] = {}
        num_mvp_dict_[this_season] = {}
        num_all_defensive_dict_[this_season] = {}
        num_dpoy_dict_[this_season] = {}

    for dict_ in nba_teams:
        for this_season in seasons_list:
            
            dict_id = dict_['id']
            
            dict_name_uncorrected = dict_['full_name']

            # print(f"{dict_id=} {dict_name_uncorrected=}")

            if dict_name_uncorrected in full_name_list:
                dict_name_corrected = dict_name_uncorrected
            else:
                dict_name_corrected = 'LA Clippers'

            num_all_star_dict_[this_season][dict_name_corrected] = 0
            num_mvp_dict_[this_season][dict_name_corrected] = 0
            num_all_defensive_dict_[this_season][dict_name_corrected] = 0
            num_dpoy_dict_[this_season][dict_name_corrected] = 0

            time.sleep(3)

            commonteamroster_ = commonteamroster.CommonTeamRoster(timeout = 3000,team_id=dict_id, season=this_season)
            team_rosters = commonteamroster_.get_data_frames()[0]
            pandas.set_option("display.max_rows", 1000)
            pandas.set_option("display.max_columns", 1000)

            player_ids = team_rosters["PLAYER_ID"]
 
            for item in player_ids:
                time.sleep(2)
                player_awards = playerawards.PlayerAwards(timeout = 3000,player_id=item)
                player_awards_df_uncorrected = player_awards.get_data_frames()[0]
                player_awards_df = player_awards_df_uncorrected[player_awards_df_uncorrected["TEAM"] == dict_name_uncorrected]

                if "NBA Most Valuable Player" in player_awards_df["DESCRIPTION"].values: # NOTE: <class 'numpy.ndarray'>
                    num_mvp_dict_[this_season][dict_name_corrected] += 1

                if "All-NBA" in player_awards_df["DESCRIPTION"].values:
                    num_all_star_dict_[this_season][dict_name_corrected] += 1

                if "NBA Defensive Player of the Year" in player_awards_df["DESCRIPTION"].values:
                    num_dpoy_dict_[this_season][dict_name_corrected] += 1

                if "All-Defensive Team" in player_awards_df["DESCRIPTION"].values:
                    num_all_defensive_dict_[this_season][dict_name_corrected] += 1


    with open("num_all_star_dict_.json", 'w') as file:
        json.dump(num_all_star_dict_, file, indent=2) 

    with open("num_mvp_dict_.json", 'w') as file:
        json.dump(num_mvp_dict_, file, indent=2) 

    with open("num_all_defensive_dict_.json", 'w') as file:
        json.dump(num_all_defensive_dict_, file, indent=2) 

    with open("num_dpoy_dict_.json", 'w') as file:
        json.dump(num_dpoy_dict_, file, indent=2) 


    # print(player_awards_dict_)

    # for item in player_awards_dict_:
    #     print(player_awards_dict_[item].get_data_frames()[0])

    # Ok so I could make one large data frame 
    # with all players in the season 
    # seperate into years 
    # seperate into each thing (mvp, defensive player of the year, all star)
    # and then group by team 


    # or maybe, add columns to each team 
    # num_all_stars, num_all_defensive, num_mvps (not finals mvp bc cyclical)

    #NBA Most Valuable Player
    #All-Defensive Team
    #NBA Defensive Player of the Year

    # TEAM_NAME
    # so i need to make a pandas df
    # in each iteration of the loop, I gain a new row
    # so team name column gets the team name (which it will merge on),
    # and num_etc (all 3 or 4) will be summed and put for the team 
    # wait, this needs to happen for each year ..... 
    # so this may not be optimal compared to prev option ? 
    # or wait still better but slower... 