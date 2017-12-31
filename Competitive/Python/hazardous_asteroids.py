'''
	My solution to https://codefights.com/challenge/5pWsEwGyJDuAeEmYX/solutions.
	@Author: Josh Snider
'''

import json
import urllib2

API_KEY = 'DEMO_KEY'
    
class Asteroid(object):
    
    def __cmp__(self, other):
        return cmp(self.name, other.name)
    
    def __init__(self, jsrc):
        self.name = jsrc[u'name']
        self.close_approach = jsrc[u'close_approach_data']
        self.hazardous = jsrc[u'is_potentially_hazardous_asteroid']
        self.jpl = jsrc[u'neo_reference_id']
        self.approaches = None
        if self.hazardous:
            self.approaches = self.close_approaches('1900-01-01', '1999-12-31')
        
    def __str__(self):
        return "{0}, {1}, {2}".format(self.name, self.jpl, str(self.approaches))
    
    def close_approaches(self, startDate, endDate):
        api_call = 'https://api.nasa.gov/neo/rest/v1/neo/{0}?api_key={1}'.format(self.jpl, API_KEY)
        req = urllib2.Request(api_call)
        response = urllib2.urlopen(req)
        j_obj = json.load(response)
        j_obj = j_obj[u'close_approach_data']
        cnt = 0
        for approach in j_obj:
            date = approach[u'close_approach_date']
            if date >= startDate and date <= endDate:
                cnt += 1
            elif date > endDate:
                break
        print(cnt)
        return cnt

def hazardousAsteroids(startDate, endDate):
    api_call = 'https://api.nasa.gov/neo/rest/v1/feed?start_date={0}&end_date={1}&api_key={2}'.format(startDate, endDate, API_KEY)
    req = urllib2.Request(api_call)
    response = urllib2.urlopen(req)
    j_obj = json.load(response)
    j_obj = j_obj[u'near_earth_objects']
    asteroids = []
    for date in j_obj:
        for ast in j_obj[date]:
            asteroids.append(Asteroid(ast))
    asteroids = [asteroid for asteroid in asteroids if asteroid.hazardous]
    max_approaches = 0
    approachers = []
    for asteroid in asteroids:
        print(asteroid)
        if asteroid.approaches > max_approaches:
            max_approaches = asteroid.approaches
            approachers = []
        if asteroid.approaches == max_approaches:
            approachers.append(asteroid)
    if len(approachers):
        approachers.sort()
        return approachers[0].name
    else:
        return "-1"
