from app import *
from flask import render_template
# from flask import Flask, flash, redirect, render_template, request, session
# from flask_session import Session
from datetime import date, datetime
import nba_api
from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguegamefinder
from nba_api.stats.endpoints import teamestimatedmetrics
import pandas 
import numpy as np
import statsmodels.api as sm
import gunicorn
import time
# from flask_cors import CORS #, cross_origin
import json
from sklearn.preprocessing import StandardScaler


def apology(message, code=400):
    """Render message as an apology to user."""
    def escape(s):
        """
        Escape special characters.

        https://github.com/jacebrowning/memegen#special-characters
        """
        for old, new in [("-", "--"), (" ", "-"), ("_", "__"), ("?", "~q"),
                         ("%", "~p"), ("#", "~h"), ("/", "~s"), ("\"", "''")]:
            s = s.replace(old, new)
        return s
    return render_template("apology.html", top=code, bottom=escape(message)), code


def pull_data():
    nba_teams = teams.get_teams()

    full_name_list = []

    for dict_ in nba_teams:
        if dict_['full_name'] != "Los Angeles Clippers":
            full_name_list.append(dict_['full_name'])
        else: 
            full_name_list.append('LA Clippers')

    gamefinder = leaguegamefinder.LeagueGameFinder()

    games = gamefinder.get_data_frames()[0]


    # Subset the games to when the last 4 digits of SEASON_ID were 2017, etc.

    games_2223 = games[games.SEASON_ID.str[-4:] == '2022']

    games_2122 = games[games.SEASON_ID.str[-4:] == '2021']

    games_2021 = games[games.SEASON_ID.str[-4:] == '2020']

    games_1920 = games[games.SEASON_ID.str[-4:] == '2019']

    games_1819 = games[games.SEASON_ID.str[-4:] == '2018']

    games_1718 = games[games.SEASON_ID.str[-4:] == '2017']

    games_1617 = games[games.SEASON_ID.str[-4:] == '2016']


    # now lets see if we can get all the games from 2016 - 2021 

    games_2223 = games_2223.loc[games_2223['TEAM_NAME'].isin(full_name_list)]

    games_2223 = games_2223[games_2223["GAME_ID"].str.startswith('002')] # **** regular season  

    games_2122 = games_2122.loc[games_2122['TEAM_NAME'].isin(full_name_list)]

    games_2122_playoffs = games_2122[games_2122["GAME_ID"].str.startswith('004')]

    games_2122 = games_2122[games_2122["GAME_ID"].str.startswith('002')] # **** regular season  

    games_2021 = games_2021.loc[games_2021['TEAM_NAME'].isin(full_name_list)]

    games_2021_playoffs = games_2021[games_2021["GAME_ID"].str.startswith('004')]

    games_2021 = games_2021[games_2021["GAME_ID"].str.startswith('002')] # **** regular season  

    games_1920 = games_1920.loc[games_1920['TEAM_NAME'].isin(full_name_list)]

    games_1920_playoffs = games_1920[games_1920["GAME_ID"].str.startswith('004')]

    games_1920 = games_1920[games_1920["GAME_ID"].str.startswith('002')] # **** regular season  

    games_1819 = games_1819.loc[games_1819['TEAM_NAME'].isin(full_name_list)]

    games_1819_playoffs = games_1819[games_1819["GAME_ID"].str.startswith('004')]

    games_1819 = games_1819[games_1819["GAME_ID"].str.startswith('002')] # **** regular season  

    games_1718 = games_1718.loc[games_1718['TEAM_NAME'].isin(full_name_list)]

    games_1718_playoffs = games_1718[games_1718["GAME_ID"].str.startswith('004')]

    games_1718 = games_1718[games_1718["GAME_ID"].str.startswith('002')] # **** regular season  

    games_1617 = games_1617.loc[games_1617['TEAM_NAME'].isin(full_name_list)]

    games_1617 = games_1617[games_1617["GAME_ID"].str.startswith('002')] # **** regular season  

    games_2223a = games_2223.groupby(['TEAM_NAME']).mean()
    games_2223w = games_2223.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='WINS')
    games_2223l = games_2223.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='L').sum()).reset_index(name='LOSSES')


    # #reset index of each DataFrame
    #games_2223a.reset_index(drop=True, inplace=True)
    games_2223w.reset_index(drop=True, inplace=True)
    games_2223l.reset_index(drop=True, inplace = True)

    games_2223_wl = pandas.merge(games_2223w, games_2223l)

    games_2223_wl.index = games_2223_wl['TEAM_NAME']

    games_2223c = games_2223a.join(games_2223_wl)

    games_2223c['WINS']=games_2223c['WINS'].astype(int)
    games_2223c['LOSSES']=games_2223c['LOSSES'].astype(int)


    # 2122:
    games_2122a = games_2122.groupby(['TEAM_NAME']).mean()
    games_2122w = games_2122.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='WINS')
    games_2122l = games_2122.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='L').sum()).reset_index(name='LOSSES')


    # #reset index of each DataFrame
    #games_2122a.reset_index(drop=True, inplace=True)
    games_2122w.reset_index(drop=True, inplace=True)
    games_2122l.reset_index(drop=True, inplace = True)

    games_2122_wl = pandas.merge(games_2122w, games_2122l)

    games_2122_wl.index = games_2122_wl['TEAM_NAME']

    games_2122c = games_2122a.join(games_2122_wl)

    games_2122c['WINS']=games_2122c['WINS'].astype(int)
    games_2122c['LOSSES']=games_2122c['LOSSES'].astype(int)

    games_2122c["IS_CHAMPION"] = np.where(games_2122c.index == "Golden State Warriors", True, False )

    games_2122_playoffs = games_2122_playoffs.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='PLAYOFF_WINS')

    games_2122_playoffs.reset_index(drop=True, inplace=True)
    games_2122c.reset_index(drop=True, inplace = True)

    games_2122c = pandas.merge(games_2122c, games_2122_playoffs, how='left')

    games_2122c.index = games_2122c['TEAM_NAME']

    games_2122c['PLAYOFF_WINS'] = games_2122c['PLAYOFF_WINS'].fillna(0)

    games_2122c['PLAYOFF_WINS']=games_2122c['PLAYOFF_WINS'].astype(int)


    # 2021:
    games_2021a = games_2021.groupby(['TEAM_NAME']).mean()
    games_2021w = games_2021.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='WINS')
    games_2021l = games_2021.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='L').sum()).reset_index(name='LOSSES')


    # #reset index of each DataFrame
    #games_2021a.reset_index(drop=True, inplace=True)
    games_2021w.reset_index(drop=True, inplace=True)
    games_2021l.reset_index(drop=True, inplace = True)

    games_2021_wl = pandas.merge(games_2021w, games_2021l)

    games_2021_wl.index = games_2021_wl['TEAM_NAME']

    games_2021c = games_2021a.join(games_2021_wl)

    games_2021c['WINS']=games_2021c['WINS'].astype(int)
    games_2021c['LOSSES']=games_2021c['LOSSES'].astype(int)

    games_2021c["IS_CHAMPION"] = np.where(games_2021c.index == "Milwaukee Bucks", True, False )

    games_2021_playoffs = games_2021_playoffs.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='PLAYOFF_WINS')

    games_2021_playoffs.reset_index(drop=True, inplace=True)
    games_2021c.reset_index(drop=True, inplace = True)

    games_2021c = pandas.merge(games_2021c, games_2021_playoffs, how='left')

    games_2021c.index = games_2021c['TEAM_NAME']

    games_2021c['PLAYOFF_WINS'] = games_2021c['PLAYOFF_WINS'].fillna(0)

    games_2021c['PLAYOFF_WINS']=games_2021c['PLAYOFF_WINS'].astype(int)


    # 1920
    games_1920a = games_1920.groupby(['TEAM_NAME']).mean()
    games_1920w = games_1920.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='WINS')
    games_1920l = games_1920.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='L').sum()).reset_index(name='LOSSES')


    # #reset index of each DataFrame
    #games_1920a.reset_index(drop=True, inplace=True)
    games_1920w.reset_index(drop=True, inplace=True)
    games_1920l.reset_index(drop=True, inplace = True)

    games_1920_wl = pandas.merge(games_1920w, games_1920l)

    games_1920_wl.index = games_1920_wl['TEAM_NAME']

    games_1920c = games_1920a.join(games_1920_wl)

    games_1920c['WINS']=games_1920c['WINS'].astype(int)
    games_1920c['LOSSES']=games_1920c['LOSSES'].astype(int)

    games_1920c["IS_CHAMPION"] = np.where(games_1920c.index == "Los Angeles Lakers", True, False )

    games_1920_playoffs = games_1920_playoffs.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='PLAYOFF_WINS')

    games_1920_playoffs.reset_index(drop=True, inplace=True)
    games_1920c.reset_index(drop=True, inplace = True)

    games_1920c = pandas.merge(games_1920c, games_1920_playoffs, how='left')

    games_1920c.index = games_1920c['TEAM_NAME']

    games_1920c['PLAYOFF_WINS'] = games_1920c['PLAYOFF_WINS'].fillna(0)

    games_1920c['PLAYOFF_WINS']=games_1920c['PLAYOFF_WINS'].astype(int)


    # 1819
    games_1819a = games_1819.groupby(['TEAM_NAME']).mean()
    games_1819w = games_1819.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='WINS')
    games_1819l = games_1819.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='L').sum()).reset_index(name='LOSSES')


    # #reset index of each DataFrame
    #games_1819a.reset_index(drop=True, inplace=True)
    games_1819w.reset_index(drop=True, inplace=True)
    games_1819l.reset_index(drop=True, inplace = True)

    games_1819_wl = pandas.merge(games_1819w, games_1819l)

    games_1819_wl.index = games_1819_wl['TEAM_NAME']

    games_1819c = games_1819a.join(games_1819_wl)

    games_1819c['WINS']=games_1819c['WINS'].astype(int)
    games_1819c['LOSSES']=games_1819c['LOSSES'].astype(int)

    games_1819c["IS_CHAMPION"] = np.where(games_1819c.index == "Toronto Raptors", True, False )

    games_1819_playoffs = games_1819_playoffs.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='PLAYOFF_WINS')

    games_1819_playoffs.reset_index(drop=True, inplace=True)
    games_1819c.reset_index(drop=True, inplace = True)

    games_1819c = pandas.merge(games_1819c, games_1819_playoffs, how='left')

    games_1819c.index = games_1819c['TEAM_NAME']

    games_1819c['PLAYOFF_WINS'] = games_1819c['PLAYOFF_WINS'].fillna(0)

    games_1819c['PLAYOFF_WINS']=games_1819c['PLAYOFF_WINS'].astype(int)


    # 1718
    games_1718a = games_1718.groupby(['TEAM_NAME']).mean()
    games_1718w = games_1718.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='WINS')
    games_1718l = games_1718.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='L').sum()).reset_index(name='LOSSES')


    # #reset index of each DataFrame
    #games_1718a.reset_index(drop=True, inplace=True)
    games_1718w.reset_index(drop=True, inplace=True)
    games_1718l.reset_index(drop=True, inplace = True)

    games_1718_wl = pandas.merge(games_1718w, games_1718l)

    games_1718_wl.index = games_1718_wl['TEAM_NAME']

    games_1718c = games_1718a.join(games_1718_wl)

    games_1718c['WINS']=games_1718c['WINS'].astype(int)
    games_1718c['LOSSES']=games_1718c['LOSSES'].astype(int)

    games_1718c["IS_CHAMPION"] = np.where(games_1718c.index == "Golden State Warriors", True, False )

    games_1718_playoffs = games_1718_playoffs.groupby('TEAM_NAME')['WL'].apply(lambda x: (x=='W').sum()).reset_index(name='PLAYOFF_WINS')

    games_1718_playoffs.reset_index(drop=True, inplace=True)
    games_1718c.reset_index(drop=True, inplace = True)

    games_1718c = pandas.merge(games_1718c, games_1718_playoffs, how='left')

    games_1718c.index = games_1718c['TEAM_NAME']

    games_1718c['PLAYOFF_WINS'] = games_1718c['PLAYOFF_WINS'].fillna(0)

    games_1718c['PLAYOFF_WINS']=games_1718c['PLAYOFF_WINS'].astype(int)


    #Merging dataframes w all_defensive info I got from API and saved as JSON
    with open('num_all_defensive_dict_.json') as file:
        num_all_defensive_dict_ = json.load(file)  


    # 2022:
    num_all_defensive_dict_2223 = pandas.DataFrame(list(num_all_defensive_dict_["2022"].items()))

    num_all_defensive_dict_2223.columns = ['TEAM_NAME', 'NUM_ALL_DEFENSIVE_PLAYERS']

    num_all_defensive_dict_2223.reset_index(drop = True, inplace = True)
    games_2223c.reset_index(drop = True, inplace = True)

    games_2223c = pandas.merge(games_2223c, num_all_defensive_dict_2223, on='TEAM_NAME')

    games_2223c.index = games_2223c['TEAM_NAME']


    # 2021:
    num_all_defensive_dict_2122 = pandas.DataFrame(list(num_all_defensive_dict_["2021"].items()))

    num_all_defensive_dict_2122.columns = ['TEAM_NAME', 'NUM_ALL_DEFENSIVE_PLAYERS']

    num_all_defensive_dict_2122.reset_index(drop = True, inplace = True)
    games_2122c.reset_index(drop = True, inplace = True)

    games_2122c = pandas.merge(games_2122c, num_all_defensive_dict_2122, on='TEAM_NAME')

    games_2122c.index = games_2122c['TEAM_NAME']


    #2020:
    num_all_defensive_dict_2021 = pandas.DataFrame(list(num_all_defensive_dict_["2020"].items()))

    num_all_defensive_dict_2021.columns = ['TEAM_NAME', 'NUM_ALL_DEFENSIVE_PLAYERS']

    num_all_defensive_dict_2021.reset_index(drop = True, inplace = True)
    games_2021c.reset_index(drop = True, inplace = True)

    games_2021c = pandas.merge(games_2021c, num_all_defensive_dict_2021, on='TEAM_NAME')

    games_2021c.index = games_2021c['TEAM_NAME']


    #2019:
    num_all_defensive_dict_1920 = pandas.DataFrame(list(num_all_defensive_dict_["2019"].items()))

    num_all_defensive_dict_1920.columns = ['TEAM_NAME', 'NUM_ALL_DEFENSIVE_PLAYERS']

    num_all_defensive_dict_1920.reset_index(drop = True, inplace = True)
    games_1920c.reset_index(drop = True, inplace = True)

    games_1920c = pandas.merge(games_1920c, num_all_defensive_dict_1920, on='TEAM_NAME')

    games_1920c.index = games_1920c['TEAM_NAME']


    #2018:
    num_all_defensive_dict_1819 = pandas.DataFrame(list(num_all_defensive_dict_["2018"].items()))

    num_all_defensive_dict_1819.columns = ['TEAM_NAME', 'NUM_ALL_DEFENSIVE_PLAYERS']

    num_all_defensive_dict_1819.reset_index(drop = True, inplace = True)
    games_1819c.reset_index(drop = True, inplace = True)

    games_1819c = pandas.merge(games_1819c, num_all_defensive_dict_1819, on='TEAM_NAME')

    games_1819c.index = games_1819c['TEAM_NAME']


    #2017:
    num_all_defensive_dict_1718 = pandas.DataFrame(list(num_all_defensive_dict_["2017"].items()))

    num_all_defensive_dict_1718.columns = ['TEAM_NAME', 'NUM_ALL_DEFENSIVE_PLAYERS']

    num_all_defensive_dict_1718.reset_index(drop = True, inplace = True)
    games_1718c.reset_index(drop = True, inplace = True)

    games_1718c = pandas.merge(games_1718c, num_all_defensive_dict_1718, on='TEAM_NAME')

    games_1718c.index = games_1718c['TEAM_NAME']


    #Merging dataframes w all_star info I got from API and saved as JSON
    with open('num_all_star_dict_.json') as file:
        num_all_star_dict_ = json.load(file)  


    # 2022:
    num_all_star_dict_2223 = pandas.DataFrame(list(num_all_star_dict_["2022"].items()))

    num_all_star_dict_2223.columns = ['TEAM_NAME', 'NUM_ALL_STAR_PLAYERS']

    num_all_star_dict_2223.reset_index(drop = True, inplace = True)
    games_2223c.reset_index(drop = True, inplace = True)

    games_2223c = pandas.merge(games_2223c, num_all_star_dict_2223, on='TEAM_NAME')

    games_2223c.index = games_2223c['TEAM_NAME']


    # 2021:
    num_all_star_dict_2122 = pandas.DataFrame(list(num_all_star_dict_["2021"].items()))

    num_all_star_dict_2122.columns = ['TEAM_NAME', 'NUM_ALL_STAR_PLAYERS']

    num_all_star_dict_2122.reset_index(drop = True, inplace = True)
    games_2122c.reset_index(drop = True, inplace = True)

    games_2122c = pandas.merge(games_2122c, num_all_star_dict_2122, on='TEAM_NAME')

    games_2122c.index = games_2122c['TEAM_NAME']


    #2020:
    num_all_star_dict_2021 = pandas.DataFrame(list(num_all_star_dict_["2020"].items()))

    num_all_star_dict_2021.columns = ['TEAM_NAME', 'NUM_ALL_STAR_PLAYERS']

    num_all_star_dict_2021.reset_index(drop = True, inplace = True)
    games_2021c.reset_index(drop = True, inplace = True)

    games_2021c = pandas.merge(games_2021c, num_all_star_dict_2021, on='TEAM_NAME')

    games_2021c.index = games_2021c['TEAM_NAME']


    #2019:
    num_all_star_dict_1920 = pandas.DataFrame(list(num_all_star_dict_["2019"].items()))

    num_all_star_dict_1920.columns = ['TEAM_NAME', 'NUM_ALL_STAR_PLAYERS']

    num_all_star_dict_1920.reset_index(drop = True, inplace = True)
    games_1920c.reset_index(drop = True, inplace = True)

    games_1920c = pandas.merge(games_1920c, num_all_star_dict_1920, on='TEAM_NAME')

    games_1920c.index = games_1920c['TEAM_NAME']


    #2018:
    num_all_star_dict_1819 = pandas.DataFrame(list(num_all_star_dict_["2018"].items()))

    num_all_star_dict_1819.columns = ['TEAM_NAME', 'NUM_ALL_STAR_PLAYERS']

    num_all_star_dict_1819.reset_index(drop = True, inplace = True)
    games_1819c.reset_index(drop = True, inplace = True)

    games_1819c = pandas.merge(games_1819c, num_all_star_dict_1819, on='TEAM_NAME')

    games_1819c.index = games_1819c['TEAM_NAME']


    #2017:
    num_all_star_dict_1718 = pandas.DataFrame(list(num_all_star_dict_["2017"].items()))

    num_all_star_dict_1718.columns = ['TEAM_NAME', 'NUM_ALL_STAR_PLAYERS']

    num_all_star_dict_1718.reset_index(drop = True, inplace = True)
    games_1718c.reset_index(drop = True, inplace = True)

    games_1718c = pandas.merge(games_1718c, num_all_star_dict_1718, on='TEAM_NAME')

    games_1718c.index = games_1718c['TEAM_NAME']


    #Merging dataframes w all_dpoy info I got from API and saved as JSON
    with open('num_dpoy_dict_.json') as file:
        num_dpoy_dict_ = json.load(file)  


    # 2022:
    num_dpoy_dict_2223 = pandas.DataFrame(list(num_dpoy_dict_["2022"].items()))

    num_dpoy_dict_2223.columns = ['TEAM_NAME', 'NUM_DPOY_PLAYERS']

    num_dpoy_dict_2223.reset_index(drop = True, inplace = True)
    games_2223c.reset_index(drop = True, inplace = True)

    games_2223c = pandas.merge(games_2223c, num_dpoy_dict_2223, on='TEAM_NAME')

    games_2223c.index = games_2223c['TEAM_NAME']


    # 2021:
    num_dpoy_dict_2122 = pandas.DataFrame(list(num_dpoy_dict_["2021"].items()))

    num_dpoy_dict_2122.columns = ['TEAM_NAME', 'NUM_DPOY_PLAYERS']

    num_dpoy_dict_2122.reset_index(drop = True, inplace = True)
    games_2122c.reset_index(drop = True, inplace = True)

    games_2122c = pandas.merge(games_2122c, num_dpoy_dict_2122, on='TEAM_NAME')

    games_2122c.index = games_2122c['TEAM_NAME']


    #2020:
    num_dpoy_dict_2021 = pandas.DataFrame(list(num_dpoy_dict_["2020"].items()))

    num_dpoy_dict_2021.columns = ['TEAM_NAME', 'NUM_DPOY_PLAYERS']

    num_dpoy_dict_2021.reset_index(drop = True, inplace = True)
    games_2021c.reset_index(drop = True, inplace = True)

    games_2021c = pandas.merge(games_2021c, num_dpoy_dict_2021, on='TEAM_NAME')

    games_2021c.index = games_2021c['TEAM_NAME']


    #2019:
    num_dpoy_dict_1920 = pandas.DataFrame(list(num_dpoy_dict_["2019"].items()))

    num_dpoy_dict_1920.columns = ['TEAM_NAME', 'NUM_DPOY_PLAYERS']

    num_dpoy_dict_1920.reset_index(drop = True, inplace = True)
    games_1920c.reset_index(drop = True, inplace = True)

    games_1920c = pandas.merge(games_1920c, num_dpoy_dict_1920, on='TEAM_NAME')

    games_1920c.index = games_1920c['TEAM_NAME']


    #2018:
    num_dpoy_dict_1819 = pandas.DataFrame(list(num_dpoy_dict_["2018"].items()))

    num_dpoy_dict_1819.columns = ['TEAM_NAME', 'NUM_DPOY_PLAYERS']

    num_dpoy_dict_1819.reset_index(drop = True, inplace = True)
    games_1819c.reset_index(drop = True, inplace = True)

    games_1819c = pandas.merge(games_1819c, num_dpoy_dict_1819, on='TEAM_NAME')

    games_1819c.index = games_1819c['TEAM_NAME']


    #2017:
    num_dpoy_dict_1718 = pandas.DataFrame(list(num_dpoy_dict_["2017"].items()))

    num_dpoy_dict_1718.columns = ['TEAM_NAME', 'NUM_DPOY_PLAYERS']

    num_dpoy_dict_1718.reset_index(drop = True, inplace = True)
    games_1718c.reset_index(drop = True, inplace = True)

    games_1718c = pandas.merge(games_1718c, num_dpoy_dict_1718, on='TEAM_NAME')

    games_1718c.index = games_1718c['TEAM_NAME']


    #Merging dataframes w mvp info I got from API and saved as JSON
    with open('num_mvp_dict_.json') as file:
        num_mvp_dict_ = json.load(file)  


    # 2022:
    num_mvp_dict_2223 = pandas.DataFrame(list(num_mvp_dict_["2022"].items()))

    num_mvp_dict_2223.columns = ['TEAM_NAME', 'NUM_MVP_PLAYERS']

    num_mvp_dict_2223.reset_index(drop = True, inplace = True)
    games_2223c.reset_index(drop = True, inplace = True)

    games_2223c = pandas.merge(games_2223c, num_mvp_dict_2223, on='TEAM_NAME')

    games_2223c.index = games_2223c['TEAM_NAME']


    # 2021:
    num_mvp_dict_2122 = pandas.DataFrame(list(num_mvp_dict_["2021"].items()))

    num_mvp_dict_2122.columns = ['TEAM_NAME', 'NUM_MVP_PLAYERS']

    num_mvp_dict_2122.reset_index(drop = True, inplace = True)
    games_2122c.reset_index(drop = True, inplace = True)

    games_2122c = pandas.merge(games_2122c, num_mvp_dict_2122, on='TEAM_NAME')

    games_2122c.index = games_2122c['TEAM_NAME']


    #2020:
    num_mvp_dict_2021 = pandas.DataFrame(list(num_mvp_dict_["2020"].items()))

    num_mvp_dict_2021.columns = ['TEAM_NAME', 'NUM_MVP_PLAYERS']

    num_mvp_dict_2021.reset_index(drop = True, inplace = True)
    games_2021c.reset_index(drop = True, inplace = True)

    games_2021c = pandas.merge(games_2021c, num_mvp_dict_2021, on='TEAM_NAME')

    games_2021c.index = games_2021c['TEAM_NAME']


    #2019:
    num_mvp_dict_1920 = pandas.DataFrame(list(num_mvp_dict_["2019"].items()))

    num_mvp_dict_1920.columns = ['TEAM_NAME', 'NUM_MVP_PLAYERS']

    num_mvp_dict_1920.reset_index(drop = True, inplace = True)
    games_1920c.reset_index(drop = True, inplace = True)

    games_1920c = pandas.merge(games_1920c, num_mvp_dict_1920, on='TEAM_NAME')

    games_1920c.index = games_1920c['TEAM_NAME']


    #2018:
    num_mvp_dict_1819 = pandas.DataFrame(list(num_mvp_dict_["2018"].items()))

    num_mvp_dict_1819.columns = ['TEAM_NAME', 'NUM_MVP_PLAYERS']

    num_mvp_dict_1819.reset_index(drop = True, inplace = True)
    games_1819c.reset_index(drop = True, inplace = True)

    games_1819c = pandas.merge(games_1819c, num_mvp_dict_1819, on='TEAM_NAME')

    games_1819c.index = games_1819c['TEAM_NAME']


    #2017:
    num_mvp_dict_1718 = pandas.DataFrame(list(num_mvp_dict_["2017"].items()))

    num_mvp_dict_1718.columns = ['TEAM_NAME', 'NUM_MVP_PLAYERS']

    num_mvp_dict_1718.reset_index(drop = True, inplace = True)
    games_1718c.reset_index(drop = True, inplace = True)

    games_1718c = pandas.merge(games_1718c, num_mvp_dict_1718, on='TEAM_NAME')

    games_1718c.index = games_1718c['TEAM_NAME']


    # Merging estimated team metrics to data frames 


    # 2022-23
    time.sleep(2)
    estimated_metrics_2223 = teamestimatedmetrics.TeamEstimatedMetrics(season="2022-23")

    estimated_metrics_2223_df = estimated_metrics_2223.get_data_frames()[0]

    estimated_metrics_2223_df.reset_index(drop = True, inplace = True)
    games_2223c.reset_index(drop = True, inplace = True)

    games_2223c = pandas.merge(games_2223c, estimated_metrics_2223_df, on='TEAM_NAME')

    games_2223c.index = games_2223c['TEAM_NAME']


    # 2021-22
    time.sleep(2)
    estimated_metrics_2122 = teamestimatedmetrics.TeamEstimatedMetrics(season="2021-22")

    estimated_metrics_2122_df = estimated_metrics_2122.get_data_frames()[0]

    estimated_metrics_2122_df.reset_index(drop = True, inplace = True)
    games_2122c.reset_index(drop = True, inplace = True)

    games_2122c = pandas.merge(games_2122c, estimated_metrics_2122_df, on='TEAM_NAME')

    games_2122c.index = games_2122c['TEAM_NAME']


    # 2020-21
    time.sleep(2)
    estimated_metrics_2021 = teamestimatedmetrics.TeamEstimatedMetrics(season="2020-21")

    estimated_metrics_2021_df = estimated_metrics_2021.get_data_frames()[0]

    estimated_metrics_2021_df.reset_index(drop = True, inplace = True)
    games_2021c.reset_index(drop = True, inplace = True)

    games_2021c = pandas.merge(games_2021c, estimated_metrics_2021_df, on='TEAM_NAME')

    games_2021c.index = games_2021c['TEAM_NAME']


    # 2019-20
    time.sleep(2)
    estimated_metrics_1920 = teamestimatedmetrics.TeamEstimatedMetrics(season="2019-20")

    estimated_metrics_1920_df = estimated_metrics_1920.get_data_frames()[0]

    estimated_metrics_1920_df.reset_index(drop = True, inplace = True)
    games_1920c.reset_index(drop = True, inplace = True)

    games_1920c = pandas.merge(games_1920c, estimated_metrics_1920_df, on='TEAM_NAME')

    games_1920c.index = games_1920c['TEAM_NAME']


    # 2018-19
    time.sleep(2)
    estimated_metrics_1819 = teamestimatedmetrics.TeamEstimatedMetrics(season="2018-19")

    estimated_metrics_1819_df = estimated_metrics_1819.get_data_frames()[0]

    estimated_metrics_1819_df.reset_index(drop = True, inplace = True)
    games_1819c.reset_index(drop = True, inplace = True)

    games_1819c = pandas.merge(games_1819c, estimated_metrics_1819_df, on='TEAM_NAME')

    games_1819c.index = games_1819c['TEAM_NAME']


    # 2017-18
    time.sleep(2)
    estimated_metrics_1718 = teamestimatedmetrics.TeamEstimatedMetrics(season="2017-18")

    estimated_metrics_1718_df = estimated_metrics_1718.get_data_frames()[0]

    estimated_metrics_1718_df.reset_index(drop = True, inplace = True)
    games_1718c.reset_index(drop = True, inplace = True)

    games_1718c = pandas.merge(games_1718c, estimated_metrics_1718_df, on='TEAM_NAME')

    games_1718c.index = games_1718c['TEAM_NAME']

    historical_years_train_data = pandas.concat([games_2122c, games_2021c, games_1920c, games_1819c, games_1718c])

    historical_years_train_data["eFG_PCT"] = (historical_years_train_data.iloc[:,3] + (0.5 * historical_years_train_data.iloc[:,6])) / historical_years_train_data.iloc[:,4]

    games_2223c["eFG_PCT"] = (games_2223c.iloc[:,3] + (0.5 * games_2223c.iloc[:,6])) / games_2223c.iloc[:,4]

    scaler = StandardScaler().set_output(transform="pandas")

    X_sk = historical_years_train_data.loc[:,['PTS', 'FGM', "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "OREB", "DREB", "REB", "AST", "STL", "BLK", "TOV", "PF", "PLUS_MINUS", "NUM_ALL_DEFENSIVE_PLAYERS", "NUM_ALL_STAR_PLAYERS", "NUM_DPOY_PLAYERS", "NUM_MVP_PLAYERS", "W_PCT", "E_OFF_RATING", "E_DEF_RATING", "E_NET_RATING", "E_PACE", "E_AST_RATIO", "E_OREB_PCT", "E_DREB_PCT", "E_REB_PCT", "E_TM_TOV_PCT", "eFG_PCT"]]

    X_sk = scaler.fit_transform(X_sk)

    y_sk = pandas.get_dummies(historical_years_train_data.IS_CHAMPION).iloc[:, 1] # dummy encoding

    y_sk_tpot = historical_years_train_data.PLAYOFF_WINS

    current_year_test_data = games_2223c.loc[:,['PTS', 'FGM', "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "OREB", "DREB", "REB", "AST", "STL", "BLK", "TOV", "PF", "PLUS_MINUS", "NUM_ALL_DEFENSIVE_PLAYERS", "NUM_ALL_STAR_PLAYERS", "NUM_DPOY_PLAYERS", "NUM_MVP_PLAYERS", "W_PCT", "E_OFF_RATING", "E_DEF_RATING", "E_NET_RATING", "E_PACE", "E_AST_RATIO", "E_OREB_PCT", "E_DREB_PCT", "E_REB_PCT", "E_TM_TOV_PCT", "eFG_PCT"]]

    current_year_test_data = scaler.fit_transform(current_year_test_data)

    current_year_test_data.to_json(path_or_buf = "current_year_test_data.json")

    y_sk.reset_index(drop = True, inplace=True)
    y_sk.to_json(path_or_buf="y_sk.json")

    y_sk_tpot.reset_index(drop = True, inplace=True)
    y_sk_tpot.to_json(path_or_buf="y_sk_tpot.json")

    X_sk.reset_index(drop = True, inplace=True)
    X_sk.to_json(path_or_buf='X_sk.json')

    time_dict = {}

    time_dict['pull_date'] = str(date.today())

    with open("time_dict.json", 'w') as file:
        json.dump(time_dict, file, indent=2) # again, indent optional

    return 


def pull_from_json(file_path, type_ = "DataFrame"):
    with open(file_path) as file:
        reconstructed_2 = json.load(file) 

    if type_ == "DataFrame":
        new_df = pandas.DataFrame(reconstructed_2)
    elif type_ == 'Series':
        new_df = pandas.Series(reconstructed_2)
    else:
        raise ValueError(f"Type {type_} not supported")

    return new_df

def should_we_query_API():
    todays_date = date.today()

    last_day_of_regular_season = datetime.strptime("04-09-2023", '%m-%d-%Y').date()

    if todays_date > last_day_of_regular_season:
        return False
    
    with open('time_dict.json') as file:
        reconstructed_time_dict = json.load(file) 

    if reconstructed_time_dict['pull_date'] == str(todays_date):
        return False
    
    return True

