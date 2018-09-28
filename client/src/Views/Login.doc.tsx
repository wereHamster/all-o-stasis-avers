import * as React from 'react'

import {markdown, ReactSpecimen} from '@catalog/core'

import {Form, AwaitingConfirmation} from './Login'

export default () => markdown`
# Form (empty, email entered, submitting)

${
<ReactSpecimen noSource span={2}>
    <Form
        email=''
        onChangeEmail={() => {}}
        doLogin={() => {}}
        isSubmitting={false}
    />
</ReactSpecimen>
}

${
<ReactSpecimen noSource span={2}>
    <Form
        email='tomas.carnecky@gmail.com'
        onChangeEmail={() => {}}
        doLogin={() => {}}
        isSubmitting={false}
    />
</ReactSpecimen>
}

${
<ReactSpecimen noSource span={2}>
    <Form
        email='tomas.carnecky@gmail.com'
        onChangeEmail={() => {}}
        doLogin={() => {}}
        isSubmitting={true}
    />
</ReactSpecimen>
}

# AwaitingConfirmation

${
<ReactSpecimen noSource>
    <AwaitingConfirmation
        email='tomas.carnecky@gmail.com'
        onReset={() => {}}
        securityCode='Blauer Truthan'
    />
</ReactSpecimen>
}
`
