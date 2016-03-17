package jcl.compiler.struct.specialoperator.declare;

public class LispNameDeclarationStruct implements DeclarationStruct {

	private final String className;

	public LispNameDeclarationStruct(final String className) {
		this.className = className;
	}

	public String getClassName() {
		return className;
	}
}
