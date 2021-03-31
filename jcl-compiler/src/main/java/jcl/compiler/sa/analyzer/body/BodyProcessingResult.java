package jcl.compiler.sa.analyzer.body;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class BodyProcessingResult {
	private final List<LispStruct> declares;
	private final StringStruct docString;
	private final List<LispStruct> bodyForms;
}
