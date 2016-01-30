/// <reference path="../../ext/react.d.ts" />

import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../../app';
import {Boulder} from '../../storage';

export interface CardProps {
    boulderE : Avers.Editable<Boulder>;
    app: App;
}

export function tileHeader(app: App, boulderId: string) : JSX.Element {

    var colorClass = "unknown";

    let meta : JSX.Element = Avers.lookupEditable<Boulder>(
      app.data.aversH, boulderId).fmap(boulderE => {
        var boulder = boulderE.content;
        var setters = '[';

        colorClass = boulder.grade;

        // let setterC = Avers.lookupEditable<bt.Storage.Account>(bt.data.aversH, boulder.setter[i]);
        //setterC.then(s => { setters += s + ' '; });

        setters += ']';

        var bDate = boulder.date;
        //bDate.getDay() + '.' + bDate.getMonth()+1 + '.' + bDate.getFullYear()

        return (
            <div className="boulder-tile-header-meta">
              <div>{'' + boulder.name + ' :: ' + boulder.sector}</div>
              <div>{bDate}</div>
            </div>
        );
    }).get(undefined);

    var boulderClass = 'boulder-title-header ' + colorClass;
    var boulderLink  = navigateToFn('/boulder/' + boulderId);
    var boulderName  = boulderId.substring(0, 5); //FIXME: replace with name

    return (
        <div className={boulderClass} onClick={boulderLink}>
          <div className="boulder-tile-header-indicator"></div>
          <div className="boulder-tile-header-title">{boulderName}</div>
          {meta}
        </div>
    );
}

function tileInfo(app: App, boulderId: string) : JSX.Element {

    let info : JSX.Element =
        Avers.lookupEditable<Boulder>(app.data.aversH, boulderId).fmap(boulderE => {
        var boulder = boulderE.content;

        return (
          <div className="boulder-card-body-paragraph centered">
            <div>{'' + boulder.gradeNr + ' :: ' + ' [ setters ]'}</div>
          </div>
        );
    }).get(undefined);

    return (
        <div className="boulder-tile-info">{info}</div>
    );
}

function tileStats(app: App, boulderId: string) : JSX.Element {

    let stats :JSX.Element =
        Avers.lookupEditable<Boulder>(app.data.aversH, boulderId).fmap(boulderE => {
        var boulder = boulderE.content;

        return (
            <div>"stats"</div>
        );
    }).get(undefined);

    return (
        <div className="boulder-tile-stats">stats</div>
    );
}

enum CardBody
    { Home
    }

interface CardState {
    cardBody: CardBody;
}

class CardSpec extends React.Component<CardProps, CardState> {

    private app : App;

    initialState(props: CardProps): CardState {
        // let boulder = props.boulderE.content;
        return { cardBody: CardBody.Home };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    render() {
        var boulderId = this.props.boulderE.objectId;

        return (
            <div className="boulder-card">
              {tileHeader(this.props.app, boulderId)}
              {tileInfo(this.props.app, boulderId)}
              {tileStats(this.props.app, boulderId)}
            </div>
        );
    }
}

export var Card = React.createFactory(CardSpec);
