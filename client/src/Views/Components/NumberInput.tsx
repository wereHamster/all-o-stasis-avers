import * as React from 'react';
import * as ReactDOM from 'react-dom';

export interface NumberInputProps {
    object : any;
    field  : string;
}

interface NumberInputState {
    rawValue : string;
}

// The value is only valid if it can be fully parsed into a number.
function 
isValidNumber(value: string): boolean {
    var num = parseFloat(value);
    return !isNaN(num) && value == '' + num;
}

function 
asString(value) {
    if (value === null || value === undefined) {
        return '';
    } else {
        return value;
    }
}

class NumberInputSpec extends React.Component<NumberInputProps, NumberInputState> {

    initialState(props) {
        var rawValue = props.object[props.field];
        return { rawValue : asString(rawValue) };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    onChange = (e: React.FormEvent<any>) => {
        let value = (e.target as HTMLInputElement).value;

        this.setState({ rawValue: value });
        if (isValidNumber(value)) {
            this.props.object[this.props.field] = parseFloat(value);
        }
    };

    render() {
        let className = '';
        if (!isValidNumber(this.state.rawValue)) {
            className = 'invalid';
        }

        function onClick(e) {
            e.stopPropagation();
        }

        return <input type="text" className={className} value={this.state.rawValue}
                      onChange={this.onChange} onClick={onClick} />;
    }

    componentWillReceiveProps(props) {
        if (ReactDOM.findDOMNode(this) !== document.activeElement) {
            this.setState(this.initialState(props));
        }
    }
}

export var NumberInput = React.createFactory(NumberInputSpec);
