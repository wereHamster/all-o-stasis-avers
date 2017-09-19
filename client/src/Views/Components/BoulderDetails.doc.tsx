import * as Avers from 'avers'
import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'

import {App, Data, infoTable} from '../../app'
import {Boulder} from '../../storage'

import {BoulderDetails} from './BoulderDetails'
import {aversH, app, boulder1Id} from '../../Catalog/App'

export default () => markdown`
This is used on the Boulder page when the user is *not* logged in.

${
<ReactSpecimen noSource>
    <BoulderDetails boulder={Avers.lookupContent<Boulder>(aversH, boulder1Id).get(undefined)} />
</ReactSpecimen>
}
`
