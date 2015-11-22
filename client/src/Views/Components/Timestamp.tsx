
export interface TimestampProps {
    object : any;
    field  : string;
}

interface TimestampState {
    rawValue : string;
}

function isValidTimestamp(value: string): boolean {
    var parsed = parseTimestamp(value);
    return parsed !== undefined;
}

function parseTimestamp(value: string): string {
    var d = Date.parse(value);
    if (!isNaN(d)) {
        return new Date(d).toISOString();
    }
}

function asString(value): string {
    if (value === null || value === undefined) {
        return '';
    } else {
        return value;
    }
}

class TimestampSpec extends React.Component<TimestampProps, TimestampState> {

    initialState(props) {
        var rawValue = props.object[props.field];
        return { rawValue : asString(rawValue) };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    onChange = (e: __React.FormEvent) => {
        let value = (e.target as HTMLInputElement).value;

        this.setState({ rawValue: value });
        var parsed = parseTimestamp(value);
        if (isValidTimestamp(value)) {
            this.props.object[this.props.field] = parsed;
        }
    };

    render() {
        let className = 'wide';
        if (!isValidTimestamp(this.state.rawValue)) {
            className += ' invalid';
        }

        function onClick(e) {
            e.stopPropagation();
        }

        return <input type="text" className={className} value={this.state.rawValue} onChange={this.onChange} onClick={onClick} />;
    }
}

export var Timestamp = React.createFactory(TimestampSpec);
