import * as Avers from 'avers';
import * as React from 'react';

import {App} from '../../app';

import {navBar} from './NavBar';

export const Site = ({app, children} : {app: App, children?: JSX.Element}) => {
    function onClick() {
        Avers.startNextGeneration(app.data.aversH)
    }

    return (
        <div className="site" onClick={onClick}>
            {navBar(app)}
            <div>{children}</div>
            <TransientNotification app={app} />
        </div>
    )
}

const TransientNotification = ({app}: {app: App}) => {
    const localChanges = Avers.localChanges(app.data.aversH)

    if (Avers.networkRequests(app.data.aversH).length > 0) {
        return (
            <div className="transient-notification">
                Saving...
            </div>
        )

    } else if (localChanges.length > 0) {
        var numLocalChanges = localChanges.reduce((a, x) => a + x.changes.length, 0)

        return (
            <div className="transient-notification">
                {'' + numLocalChanges + ' unsaved changes'}
            </div>
        )

    } else {
        return (
            <div className="transient-notification" />
        )
    }
}
