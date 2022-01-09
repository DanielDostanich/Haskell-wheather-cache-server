# Haskell wheather cache server

## Eng

This app is a cache server for storing wheater data from OpenWheaterMap. Server can get data by city, city id (it can be founded here http://bulk.openweathermap.org/sample/), ZIP code or by coordinates. Also supports automatic updates for sertain pre-given locations.

### Server requests

By city

    /city?q={City name}
    /city?q=London

By city id

    /cityid/?id={ID}
    /cityid/?id=2172797
    
By ZIP code

    /zipcode/?zip={ZIP code}
    /zipcode/?zip=94040,us

By coordinates

    /coords/?lat={lattitude}&lon={longtitude}
    /coords/?lat=35&lon=139

### Server setup

OpenWheaterMap API key is passed via command line arguments

    stack run -- --api {key}

All other parameters are passed via a configuration file config/congif.yaml

#### Parameters

port: server port

    port: 8004

timeError: optional, an acceptable time error, measurment unit -- seconds, default value -- 1200 (20 minutes)

    timeError: 3600

rangeError: optional, an acceptable range error, measurment unit -- kilometers, default value -- 30

    rangeError: 100

updateTime: optional, time gap between automatic updates, measurment unit -- seconds, default value -- 1800 (30 minutes)

    udpateTime: 3600

locations: a list of locations to be automatically updated

##### Location data format for the configuration file

typ: type of a request

    typ = "city" | "cityId" | "zipCode" | "coords"

val: only for city name, ZIP code and city id:
- "city": city name
- "cityId": city id
- "zipCode": ZIP code of a location

        - typ: city
          val: London
        - typ: cityId
          val: "2172797"
        - typ: zipCode
          val: "94040,us"

lat/lon: for locations by coordinates. Latitude/longitude accordingly

        - typ: coords
          lat: "35"
          lon: "139"

### Internal organization

For storing data the app uses Redis. There are 4 structures:
- Temperature_Ordered_Set : Ordered Set
- Cities_Coords_Map : HashMap
- CitiesIDs_Coords_Map : HashMap
- zipcode_Coords_Map : HashMap

The first one is used for storing wheather JSONs by coordinates,  the remaining thee are for matching places with coordinates (location data -> its coordinates). 

After receiving a request without coordinates (like request by a city) the server tries to find its coordinates in one of the maps. If the place is not found the server requests wheather data from the OpenWheatherMap, looks into the response and creates a new entry in the according map (the response contains the coordinates). In case the server has the coordination data without sending a request, it tries to find an appropriate entry it in the Temperature_Ordered_Set.

The data in the Ordered Set is ordered by longitude.

For quick search the server calculates two points, which create borders of a sector, where the appropriate result may be found. The points are longtitudes of places if one tries to walk the whole range error eastward or westward accordingly. After that the server gets all points from this sector and linearly search the nearest to the requested location (and not expired).

![alt Explanation_Picture](explanation.png)

After each request to the OpenWheatherMap servers the data stored in the set is updated.

## Rus

Приложение реализует кеш-сервер для погоды. Сервер позволяет узнать погоду по городу, id города (может быть найден тут: http://bulk.openweathermap.org/sample/), zip-коду или координатам.

### Запросы к серверу

По городу

    /city?q={Название города}
    /city?q=London

По id

    /cityid/?id={Идентификатор}
    /cityid/?id=2172797
    
По зипкоду

    /zipcode/?zip={zip-код}
    /zipcode/?zip=94040,us

По координатам 

    /coords/?lat={Широта}&lon={Долгота}
    /coords/?lat=35&lon=139

### Настройки сервера

Ключ для OpenWheaterMap передается посредством аргумента командной строки

    stack run -- --api {ключ}

Все остальные параметры настраиваются посредством конфигурационного файла config/congif.yaml

#### Параметры

port: порт, на котором будет запущен сервер

    port: 8004

timeError: опционально, допустимая погрешность для времени, еденица измерения -- секунды, значение по умолчанию -- 1200 (20 минут)

    timeError: 3600

rangeError: опционально, допустимая погрешность для локации, еденица измерения -- километры, значение по умолчанию -- 30

    rangeError: 100

updateTime: опционально, промежуток времени между автоматическими обновлениями, еденица измерения -- секунды, значение по умолчанию -- 1800 (30 минут)

    udpateTime: 3600

locations: список локаций, для которых будет производится автоматическое обновление данных

##### Формат данных локаций

typ: показывает, по каким данным будут производится запросы

    typ = "city" | "cityId" | "zipCode" | "coords"

val: для поиска по городу, id и зипкоду:
- "city": Название города
- "cityId": Идентификатор
- "zipCode": zip-код локации

        - typ: city
          val: London
        - typ: cityId
          val: "2172797"
        - typ: zipCode
          val: "94040,us"

lat/lon: для поиска по координатам. Задает широту и долготу соответственно

        - typ: coords
          lat: "35"
          lon: "139"

### Внутреннее устройство

Для хранения данных используется Redis. Всего используется 4 хранилища:
- Temperature_Ordered_Set : Ordered Set
- Cities_Coords_Map : HashMap
- CitiesIDs_Coords_Map : HashMap
- zipcode_Coords_Map : HashMap

Первое используется для хранения JSON'а с погодой по координатам, оставшиеся три задают соответствие  (Описание места -> его координаты). 

Когда происходит первый запрос локации не по координатам сервер честно отправляет запрос на сервер OpenWheatherMap и из полученого JSON'а получает координаты, которые сохраняет в HashMap'е 

Данные в Ordered Set'е хранятся упорядоченные по долготе (longitude)

Для быстрого доcтупа к данным по координатам программа изначально высчитывает две точки, которые находятся на длине растояния погрешности от данного места, если при этом идти строго на запад или восток. Теперь, если взять долготу этих точек, мы можем получить сектор, в котором обязательно будут лежать все точки, которые находятся на расстоянии меньше расстояния погрешности. 

![alt Explanation_Picture](explanation.png)

После этого из Set'а берутся все точки в указаном сегменте, и среди них линейно ищется ближайшая.

При каждом обращении к серверам OpenWheatherMap происходит обновление данных в Set'e
