package jcl.reader.state;

import org.springframework.statemachine.annotation.WithStateMachine;
import org.springframework.stereotype.Component;

@Component
@WithStateMachine
public class ReaderSMBean {

	@OnReaderTransition(target = States.READ)
	void toState1() {
	}

	@OnReaderTransition(target = States.ILLEGAL_CHARACTER)
	void toState2() {
	}
}
