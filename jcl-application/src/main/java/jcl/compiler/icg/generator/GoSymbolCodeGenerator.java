package jcl.compiler.icg.generator;

import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.struct.specialoperator.go.GoSymbolStruct;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
final class GoSymbolCodeGenerator extends GoCodeGenerator<GoSymbolStruct> {

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<GoSymbolStruct> event) {
		super.onGeneratorEvent(event);
	}
}
