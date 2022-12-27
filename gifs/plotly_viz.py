import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import plotly
import plotly.express as px
import plotly.graph_objects as go

# load data
bmb = pd.read_csv('plotly_stats.csv')
week1 = pd.read_csv('plotly_tracking.csv')
p, g = 3828, 2021091207
df = week1[(week1.playId==p)]
df = df[(df.gameId==g)]
df = df.merge(bmb, left_on='frameId', right_on='frameId', how='left') # merge df with metrics
oe = list(bmb['oe'])

# define function
def create_animation(df, oe):
    colorsIdx = {'TEN': 'rgba(81, 207, 245, 0.8)', 'ARI': 'rgba(250, 110, 129, 0.8)', 'football': 'rgba(191, 128, 80, 0.95)'}
    fig = px.scatter(df, y='x', x='y', color='team', text='jerseyNumber', animation_frame='time',
                     color_discrete_map=colorsIdx,
                     range_y=[50,120],range_x=[53.3, -10],
                     hover_data=['nflId', 'x', 'y', 's', 'a', 'dis', 'o', 'playDirection']
                    )
    fig.update_layout(
    font=dict(
        size=10
    ))
    fig.update_traces(marker_size=12)
    fig.update_layout(legend=dict(
    yanchor="top",
    y=0,
    xanchor="left",
    x=0
    ))
    fig.update_layout(paper_bgcolor='#FFFFFF', plot_bgcolor='#FFFFFF', font_color='black',
        width = 570, height = 570, title = "", xaxis = dict(
            nticks = 40, title = "", visible=False, autorange=False
        ),
        yaxis = dict(
            scaleanchor = "x", title = "Temp", visible=False, autorange=False
        ), showlegend= True, annotations=[dict(
            y=112, x=26, xref="x", yref="y", text="VISITOR ENDZONE", font=dict(size=13,color="black"),
            align='center', showarrow=False, yanchor='bottom',textangle=0
        )]
        , legend=dict(
            traceorder="normal", font=dict(family="sans-serif",size=13), title = "", orientation="h",
            yanchor="bottom", y=1.00, xanchor="center", x=0.45
        ))

    fig.add_shape(type="rect", y0=110, y1=120, x0=0, x1=53.3,line=dict(color="#808A87",width=3),fillcolor="#cbfa84" ,layer="below")
    for y in range(60, 110, 10):
        fig.add_shape(type="rect", y0=y,   y1=y+10, x0=0, x1=53.3,line=dict(color="#808A87",width=3),fillcolor="#cbfa84" ,layer="below")
    for y in range(60, 110, 1):
        fig.add_shape(type="line",y0=y, x0=1, y1=y, x1=2,line=dict(color="#808A87",width=2),layer="below")
    for y in range(60, 110, 1):
        fig.add_shape(type="line",y0=y, x0=51.3, y1=y, x1=52.3,line=dict(color="#808A87",width=2),layer="below")
    for y in range(60, 110, 1):
        fig.add_shape(type="line",y0=y, x0=20.0, y1=y, x1=21,line=dict(color="#808A87",width=2),layer="below")
    for y in range(60, 110, 1):
        fig.add_shape(type="line",y0=y, x0=32.3, y1=y, x1=33.3,line=dict(color="#808A87",width=2),layer="below")
    fig.add_trace(go.Scatter(
    y=[60,70,80,90,100,108], x=[5,5,5,5,5,5],
    text=["50","4 0","3 0","2 0","1 0","G"],
    mode="text",
    textfont=dict(size=13,family="Arail"),
    showlegend=False,
    ))
    fig.add_trace(go.Scatter(
    y=[60,70,80,90,100,108], x=[48.3,48.3,48.3,48.3,48.3,48.3],
    text=["50","4 0","3 0","2 0","1 0","G"],
    mode="text",
    textfont=dict(size=13,family="Arail"),
    showlegend=False,
    ))

    return fig

# add BMB statistic
fig = create_animation(df, oe)
for i, f in enumerate(fig.frames):
    if oe[i]>=1:
        fig.frames[i]['layout'].update(title_text=".                           Big Man Betweenness: {}".format(round(oe[i],2)), title_font_color="black")
    elif oe[i]<1:
        fig.frames[i]['layout'].update(title_text=".                           Big Man Betweenness: {}".format(round(oe[i],2)), title_font_color="#E62333")
for button in fig.layout.updatemenus[0].buttons:
    button['args'][1]['frame']['redraw'] = True

# write html
fig.write_html('./plotly_html.html')
