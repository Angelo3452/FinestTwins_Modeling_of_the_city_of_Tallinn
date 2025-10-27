import json
import uuid

# THIS FUNCTION GENERATES THE OPERATOR JSON FILE FOR A DRT-TAXI SERVICE
def gen_drt_operator_input_file(depot_stations_number, fleet_size):

    vehicles_per_station = dict()
    for i in range(depot_stations_number):
        vehicles_per_station.update({ "GARAGE "+str(i+1): int(fleet_size/depot_stations_number) })


    operator_dict = {"id": str(uuid.uuid4()), "name": "DRT-Taxi Operator", "type": "external", "address": "localhost:45001", 
    "vehicle_type": 2394298, "fleet": []}
    
    for garage, fleet_number in vehicles_per_station.items():
        for i in range(fleet_number):
            veh_dict = {"id": str(uuid.uuid4()), "name": "VEH"+str(i+1), "origin": {"object": garage}}
            operator_dict["fleet"].append(veh_dict)


    with open("operator_definition_drt.json", "w") as outfile:
        json.dump(operator_dict, outfile, indent=4)
    
    with open("operator_definition_drt.json", "w") as outfile:
        json.dump(operator_dict, outfile, indent=4)

# THIS FUNCTION GENERATES THE OPERATOR JSON FILE FOR A CARSHARING SERVICE
##def gen_carsharing_operator_input_file(stations_number, station_occupancy):
##
##    vehicles_per_station = dict()
##    for i in range(stations_number):
##        vehicles_per_station.update({ "css_"+str(i+1): station_occupancy })
##
##    operator_dict = {"id": str(uuid.uuid4()), "name": "Carsharing Operator (station-to-station)", "type": "external", 
##    "address": "localhost:45002", "vehicle_type": 2394298, "frequencySecondsRebalancing": 0, "rebalancing": 0, 
##    "rebalancingType": "UsingFleet", "fleet": [], "veh_stations": []}
##    
##    for station, capacity in vehicles_per_station.items():
##        for i in range(capacity-5):
##            veh_dict = {"id": str(uuid.uuid4()), "name": "Car No: "+str(i+1)+"_Initial Station: "+station, "origin": {"object": station}, 
##            "supportCluster": 0}
##            operator_dict["fleet"].append(veh_dict)
##        
##        station_dict = {"id": str(uuid.uuid4()), "name": station, "nickName": station, "capacity": capacity, 
##        "idCluster": 0, "depot": 0, "position": { "object": station}, "predictedDemand": [0]}
##        operator_dict["veh_stations"].append(station_dict)
##
##
##    with open("carsharing_operator/operator_definition_carsharing.json", "w") as outfile:
##        json.dump(operator_dict, outfile, indent=4)
##    
##    with open("operator_definition_carsharing.json", "w") as outfile:
##        json.dump(operator_dict, outfile, indent=4)
##
##
### THIS FUNCTION GENERATES THE OPERATOR JSON FILE FOR A BIKESHARING SERVICE
##def gen_bikesharing_operator_input_file(stations_number, station_occupancy):
##
##    vehicles_per_station = dict()
##    for i in range(stations_number):
##        vehicles_per_station.update({ "bss_"+str(i): station_occupancy })
##
##    operator_dict = {"id": str(uuid.uuid4()), "name": "Bikesharing Operator (station-to-station)", "type": "external", 
##    "address": "localhost:45003", "vehicle_type": 152, "frequencySecondsRebalancing": 0, "rebalancing": 0, 
##    "rebalancingType": "UsingFleet", "fleet": [], "veh_stations": []}
##    
##    for station, capacity in vehicles_per_station.items():
##        for i in range(capacity-5):
##            veh_dict = {"id": str(uuid.uuid4()), "name": "Car No: "+str(i)+"_Initial Station: "+station, "origin": {"object": station}, 
##            "supportCluster": 0}
##            operator_dict["fleet"].append(veh_dict)
##        
##        station_dict = {"id": str(uuid.uuid4()), "name": station, "nickName": station, "capacity": capacity, 
##        "idCluster": 0, "depot": 0, "position": { "object": station}, "predictedDemand": [0]}
##        operator_dict["veh_stations"].append(station_dict)
##
##
##    with open("bikesharing_operator/operator_definition_bikesharing.json", "w") as outfile:
##        json.dump(operator_dict, outfile, indent=4)
##    
##    with open("operator_definition_bikesharing.json", "w") as outfile:
##        json.dump(operator_dict, outfile, indent=4)



if __name__ == '__main__':
    #############################--------------------DRT---------------------------############################

    # number of stations where the DRT fleet will be loaded into the simulation (initial position of fleet vehicles)
    # NOTE: in that case, it's a hardcoded fixed number based on manually created objects created for the 
    # virtual city ang file (however a shapefile can be also loaded) - DO NOT CHANGE VALUE
    _depot_stations_number = 3

    # nummber of vehicles in the DRT fleet
    # NOTE: This value is freely configurable!
    fleet_size = 300

    gen_drt_operator_input_file(_depot_stations_number, fleet_size)
    
    
    
    #############################----------------CARSHARING--------------------############################

    # number of stations where the Carsharing fleet will be parked
    # NOTE: in that case, it's a hardcoded fixed number based on manually created objects created for the 
    # virtual city ang file (however a shapefile can be also loaded) - DO NOT CHANGE VALUE
    stations_number = 8

    # nummber of vehicles that can be parked in each station
    # NOTE: This value is freely configurable!
    station_occupancy = 15

    #NOTE: The final fleet size is not stations_number*station_occupancy since we need to make sure that there more parking
    #spots than the actual fleet; therefore each station is assumed to have 5 vehicles less than occupancy. Meaning that the
    #total fleet size is assumed to be fleet_size = stations_number*station_occupancy - (stations_number*5)
    #this value convention is hardcoded within the below fuction!!
    
##    gen_carsharing_operator_input_file(stations_number, station_occupancy)


    #############################---------------BIKESHARING---------------------############################

    # number of stations where the Bikesharing fleet will be parked
    # NOTE: in that case, it's a hardcoded fixed number based on manually created objects created for the 
    # virtual city ang file (however a shapefile can be also loaded) - DO NOT CHANGE VALUE
    stations_number = 18

    # nummber of vehicles that can be parked in each station
    # NOTE: This value is freely configurable!
    station_occupancy = 20

    #NOTE: The final fleet size is not stations_number*station_occupancy since we need to make sure that there more parking
    #spots than the actual fleet; therefore each station is assumed to have 5 vehicles less than occupancy. Meaning that the
    #total fleet size is assumed to be fleet_size = stations_number*station_occupancy - (stations_number*5)
    #this value convention is hardcoded within the below fuction!!

##    gen_bikesharing_operator_input_file(stations_number, station_occupancy)
