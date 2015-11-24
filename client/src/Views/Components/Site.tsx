import * as Avers from '../../lib/avers';
import {App} from '../../app';

export function site(app: App, ...content) {

    function onClick() {
        Avers.startNextGeneration(app.data.aversH);
    }

    var transientNotification;

    var localChanges = Avers.localChanges(app.data.aversH);

    if (Avers.networkRequests(app.data.aversH).length > 0) {
        transientNotification = <div className="transient-notification">Saving...</div>;

    } else if (localChanges.length > 0) {
        var numLocalChanges = localChanges.reduce((a, x) => {
            return a + x.changes.length;
        }, 0);

        transientNotification = <div className="transient-notification">{'' + numLocalChanges + ' unsaved changes'}</div>;
    }

    content.unshift({});

    return (
        <div className="site" onClick={onClick}>
            {React.DOM.div.apply(React.DOM, content)}
            {transientNotification}
        </div>
    );
}


export function Site({ app, children }) {
    return site(app, React.Children.toArray(children));
}

