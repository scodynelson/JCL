package jcl.lang.classes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link ClassStruct} is the object representation of a Lisp 'class' type.
 */
public abstract class ClassStruct extends StandardObjectStruct {

	private static final ConcurrentHashMap<SymbolStruct, ClassStruct> map = new ConcurrentHashMap<>();

	private final SymbolStruct name;

	private List<ClassStruct> directSuperClasses = new ArrayList<>();
	private final List<ClassStruct> directSubClasses = new ArrayList<>();
	private List<ClassStruct> classPrecedenceList = new ArrayList<>();

	protected ClassStruct(final SymbolStruct name) {
		this.name = name;
	}

	protected ClassStruct(final SymbolStruct name, final List<ClassStruct> directSuperClasses) {
		this.name = name;
		this.directSuperClasses = directSuperClasses;
	}

	public SymbolStruct getClassName() {
		return name;
	}

	public List<ClassStruct> getDirectSuperClasses() {
		return directSuperClasses;
	}

	public void setDirectSuperClasses(final ClassStruct... directSuperClasses) {
		this.directSuperClasses = Arrays.asList(directSuperClasses);
	}

	public void setDirectSuperClasses(final List<ClassStruct> directSuperClasses) {
		this.directSuperClasses = directSuperClasses;
	}

	public List<ClassStruct> getDirectSubClasses() {
		return directSubClasses;
	}

	public List<ClassStruct> getClassPrecedenceList() {
		return classPrecedenceList;
	}

	public void setClassPrecedenceList(final ClassStruct... classPrecedenceList) {
		this.classPrecedenceList = Arrays.asList(classPrecedenceList);
	}

	public void setClassPrecedenceList(final List<ClassStruct> classPrecedenceList) {
		this.classPrecedenceList = classPrecedenceList;
	}

	public static ClassStruct addClass(final SymbolStruct symbol, final ClassStruct classStruct) {
		map.put(symbol, classStruct);
		return classStruct;
	}

	public static ClassStruct findClass(final SymbolStruct symbol) {
		return map.get(symbol);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.CLASS;
	}

	@Override
	public ClassStruct classOf() {
		return StandardClassStruct.CLASS;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.CLASS) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == StandardClassStruct.CLASS) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
