import * as Avers from 'avers'
import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'

import {App, Data, infoTable} from '../../app'
import {Boulder} from '../../storage'

import {BoulderCard} from './BoulderCard'
import {aversH, app, boulder1Id} from '../../Catalog/App'

export default () => markdown`
A yellow BoulderCard.

${
<ReactSpecimen noSource>
    <BoulderCard app={app} boulderE={Avers.lookupEditable<Boulder>(aversH, boulder1Id).get(undefined as any)} />
</ReactSpecimen>
}
`
