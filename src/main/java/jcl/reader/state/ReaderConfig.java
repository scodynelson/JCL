package jcl.reader.state;

import java.util.EnumSet;

import org.springframework.context.annotation.Configuration;
import org.springframework.statemachine.config.EnableStateMachine;
import org.springframework.statemachine.config.EnumStateMachineConfigurerAdapter;
import org.springframework.statemachine.config.builders.StateMachineStateConfigurer;
import org.springframework.statemachine.config.builders.StateMachineTransitionConfigurer;

@Configuration
@EnableStateMachine
public class ReaderConfig extends EnumStateMachineConfigurerAdapter<States, Events> {

	@Override
	public void configure(final StateMachineStateConfigurer<States, Events> states) throws Exception {
		states.withStates()
		      .initial(States.READ)
		      .states(EnumSet.allOf(States.class));
	}

	@Override
	public void configure(final StateMachineTransitionConfigurer<States, Events> transitions) throws Exception {
		transitions.withExternal()
		           .source(States.READ).target(States.ILLEGAL_CHARACTER)
		           .event(Events.EVENT1)
		           .and()
		           .withExternal()
		           .source(States.ILLEGAL_CHARACTER).target(States.READ)
		           .event(Events.EVENT2);
	}
}

