import * as Avers from 'avers';
import {App, refresh, navigateTo, navigateToFn} from '../../app';
import {Account, Boulder} from '../../storage';

export function tileHeader(app: App, boulderId: string) : JSX.Element {

    var colorClass = "unknown";
    var gradeNumber = 0;

    // FIXME: group by date
    let meta : JSX.Element = Avers.lookupEditable<Boulder>(
      app.data.aversH, boulderId).fmap(boulderE => {
        var boulder = boulderE.content;

        colorClass = boulder.grade.toLowerCase();
        gradeNumber = boulder.gradeNr;

        var setters = "";
        for (var i = 0; i < boulder.setter.length; i++) {
            setters += Avers.lookupContent<Account>(app.data.aversH, boulder.setter[i]).fmap(account => {

                var name = boulder.setter[i].substring(0, 7);
                if (account.name != "")
                    name = account.name;

                return name + ', ';
            }).get(undefined);
        }

        setters = setters.substring(0, setters.length - 2);

        let boulderDate = boulderE.createdAt.getDate() + '.' + 
            (boulderE.createdAt.getMonth() + 1) + '.' +
            boulderE.createdAt.getFullYear();

        return (
            <div className="boulder-tile-header-meta">
              {boulderDate + " :: " + boulder.sector + " :: " + setters}
            </div>
        );
    }).get(undefined);

    var boulderClass = 'boulder-tile-header ' + colorClass;
    var boulderLink  = navigateToFn('/boulder/' + boulderId);

    return (
        <div className={boulderClass} onClick={boulderLink}>
          <div className="boulder-tile-header-indicator">{gradeNumber}</div>
          {meta}
        </div>
    );
}

enum CardBody
    { Home
    }

class CreatingEvent {
    constructor
        ( public createEventPromise: Promise<string>
        ) {}
}

interface CardState {
    cardBody: CardBody | CreatingEvent;
}

export interface CardProps {
    app: App;
    boulderE : Avers.Editable<Boulder>;
}

class CardSpec extends React.Component<CardProps, CardState> {

    initialState(props: CardProps): CardState {
        return { cardBody: CardBody.Home };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    render() {
        var boulderId = this.props.boulderE.objectId;

        return (
            <li className="boulder-entry">
              {tileHeader(this.props.app, boulderId)}
            </li>
        );
    }
}

export var BoulderCard = React.createFactory(CardSpec);
