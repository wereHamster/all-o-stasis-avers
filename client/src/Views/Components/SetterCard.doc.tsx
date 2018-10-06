import * as Avers from 'avers'
import * as React from 'react'

import {markdown, ReactSpecimen} from '@catalog/core'

import {Account} from '../../storage'

import {SetterCard} from './SetterCard'
import {aversH, app, setter1Id} from '../../Catalog/App'

export default () => markdown`
${
<ReactSpecimen noSource>
    <SetterCard
        app={app}
        accountId={setter1Id}
        account={undefined}
    />
</ReactSpecimen>
}

${
<ReactSpecimen noSource>
    <SetterCard
        app={app}
        accountId={setter1Id}
        account={Avers.lookupContent<Account>(aversH, setter1Id).get(undefined)}
    />
</ReactSpecimen>
}
`
