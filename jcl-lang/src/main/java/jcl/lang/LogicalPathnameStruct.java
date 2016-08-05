package jcl.lang;

/**
 * The {@link LogicalPathnameStruct} is the object representation of a Lisp 'logical-pathname' type.
 */
public interface LogicalPathnameStruct extends PathnameStruct {

	PathnameStruct translateLogicalPathname();
}
