"""
Hack to migrate data from MongoDB to RethinkDB (Avers).

To run the script, download the latest mongo data, start locally:

    sudo docker run --rm -p 27017:27017 -v $HOME/data/mongo/dump:/data/dump -v $HOME/data/mongo/db:/data/db mongo:2.6

get a valid session cookie from the browser, then run the script.
"""

import datetime
import json
import time
import requests

from pymongo import MongoClient
client = MongoClient("127.0.0.1")
db = client.minimumtwo

cookie = ""

email_mapping = {}
email_mapping['mt_mouver@gmx.ch'] = 'matthi@minimum.ch'
email_mapping['gast'] = 'matthi@minimum.ch'
email_mapping['weily@gmx.ch'] = 'weily-@hotmail.com'
email_mapping['manu@mail.ch'] = 'manuel.wacker@gmx.ch'
email_mapping['me@mail.ch'] = 'phhaenggli@gmx.net'
email_mapping['NaE'] = 'tbisig@gmail.com'
email_mapping['adi@minimum-bouldering.ch'] = 'adi@minimum.ch'
email_mapping['siegristpascal@gmail.com'] = 'pascal@minimum.ch'

# NEW:
# catharina@espuelas.ch
# aelvyr@gmail.com
# andrea.kuemin@gmx.ch



"""
{
  id: "IXdZpGSqAjI832dqJZqvHemNYi3YQmIrxg9p3KpFc4"
  type: "account"
  content: {
    login: "me@me.org"
    name: ""
    email: "me@me.org"
    role: "setter"
  }
}
"""

account_mapping = {}

for account in db.setters.find({}):
    old_id = account['_id']
    name = account['name']
    email = account['email']

    # migrate to new email if required
    if email in email_mapping.keys():
      email = email_mapping[email]

    # now insert into avers
    account_data = dict(
        type = 'account',
        content = dict (
            login = email,
            name = name,
            email = email,
            role = "setter",
        )
    )

    r = requests.post('https://api.iff.io/objects', cookies={'session': cookie}, json=account_data)
    account_mapping[str(old_id)] = json.loads(r.content)['id']
    assert r.ok

try:
    json.dump(account_mapping, open('accounts', 'w'))
except:
    print json.dumps(account_mapping)

"""
{
  id: "IXdZpGSqAjI832dqJZqvHemNYi3YQmIrxg9p3KpFc4"
  type: "boulder"
  content: {
    grade: "yellow"
    gradeNr: 0
    name: ""
    removed: 0
    sector: "spektrumone"
    setDate: 1538682392082
    setter: ["AjVOGjXIUrRIKTyQbCYTqzzCEYa7VqAeWhrGRLqg4o"]
  }
}
"""

grade_translate = dict (
    Gelb = 'yellow',
    Gruen = 'green',
    Orange = 'orange',
    Blau = 'blue',
    Rot = 'red',
    Weiss = 'white'
)

epoch = datetime.datetime.utcfromtimestamp(0)
for boulder in db.boulders.find({}):

    # FIX wrong rem date
    rem_date_year = str(boulder['removed']).split('-')[0]
    set_date_year = str(boulder['date']).split('-')[0]
    if set_date_year == '2017' and rem_date_year == '2013':
        boulder['removed'] = boulder['removed'].replace(year=2017, month=11, day=10)

    boulder_data = dict(
        type = 'boulder',
        content = dict (
            grade = grade_translate[boulder['grade']],
            removed = int((boulder['removed'] - epoch).total_seconds() * 1000) if boulder['removed'] else 0,
            setDate = int((boulder['date'] - epoch).total_seconds() * 1000),
            sector = boulder['sector'].lower().replace(' ', '').replace('1', 'one').replace('spectrum2', 'spektrumtwo').replace('3', 'three').replace('4', 'four'),
            name = boulder['name'],
            gradeNr = boulder['gradenr'],
            setter = [ account_mapping[str(accId)] for accId in boulder['setters'] ],
        )
    )

    r = requests.post('https://api.iff.io/objects', cookies={'session': cookie}, json=boulder_data)
    if not r.ok:
        print boulder_data
    assert r.ok



