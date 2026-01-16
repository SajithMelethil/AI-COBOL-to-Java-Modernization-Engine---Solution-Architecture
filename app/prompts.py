def get_translation_prompt(cobol_code: str) -> str:
    return f"""
You are a Senior Banking Systems Architect specializing in legacy modernization. Your task is to convert COBOL banking code into clean, maintainable Java code with full documentation.

COBOL SOURCE CODE TO TRANSLATE:

{cobol_code}


TRANSLATION REQUIREMENTS:

1. **Java Structure**:
   - Create a complete Java class with proper package structure
   - Use appropriate Java data types (BigDecimal for financial amounts)
   - Implement error handling with exceptions
   - Use modern Java features (Java 17+)

2. **Banking Domain Mapping**:
   - COBOL PIC 9(n) → Java BigDecimal or Integer
   - COBOL PIC X(n) → Java String
   - COBOL COMP-3 → Java BigDecimal
   - COBOL file operations → Java class methods
   - COBOL paragraphs → Java methods
   - COBOL sections → Java classes

3. **Documentation**:
   - Add comprehensive JavaDoc comments for all classes and methods
   - Include @param, @return, and @throws annotations
   - Document business logic assumptions

4. **Code Quality**:
   - Follow Java naming conventions (camelCase)
   - Use immutable objects where possible
   - Implement proper encapsulation
   - Add input validation

5. **Output Format**:
   Return a JSON object with exactly these two fields:
   {{
     "java_code": "Complete Java source code here",
     "explanation": "Plain English explanation of the translated logic, assumptions made, and any banking domain considerations"
   }}

IMPORTANT NOTES FOR BANKING SYSTEMS:
- Financial calculations must use BigDecimal for precision
- Maintain audit trails through logging
- Consider thread safety for transaction processing
- Preserve original business logic exactly
- Add appropriate null checks and validations

Begin translation now. Return ONLY the JSON object, no additional text.
"""