import * as Avers from 'avers'
import * as React from 'react'

import {App} from '../../app'

import {navBar} from './NavBar'
import {TransientNotification} from './TransientNotification'

export const Site = ({app, children}: {app: App, children?: JSX.Element | JSX.Element[]}) => {
    function onClick() {
        Avers.startNextGeneration(app.data.aversH)
    }

    return (
        <div onClick={onClick}>
            {navBar(app)}
            <div>{children}</div>
            <TransientNotification app={app} />
        </div>
    )
}
