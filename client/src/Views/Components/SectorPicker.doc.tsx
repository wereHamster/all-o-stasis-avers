import * as React from 'react'

import {markdown, ReactSpecimen} from '@catalog/core'
import {SectorPicker} from './SectorPicker'

export default () => markdown`
${<ReactSpecimen noSource>
    <SectorPicker sectors={[]} onChange={() => {}} />
</ReactSpecimen>}
`
