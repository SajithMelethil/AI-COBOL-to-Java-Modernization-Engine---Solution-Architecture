import json
import logging
from typing import Dict, Any
from app.groq_client import get_groq_client
from app.prompts import get_translation_prompt

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def parse_llm_response(response_text: str) -> Dict[str, str]:
    try:
        if "```json" in response_text:
            json_start = response_text.find("```json") + 7
            json_end = response_text.find("```", json_start)
            json_str = response_text[json_start:json_end].strip()
        elif "```" in response_text:
            json_start = response_text.find("```") + 3
            json_end = response_text.find("```", json_start)
            json_str = response_text[json_start:json_end].strip()
        else:
            json_str = response_text.strip()
        
        result = json.loads(json_str)
        
        if "java_code" not in result or "explanation" not in result:
            raise ValueError("Missing required fields in LLM response")
            
        return result
        
    except json.JSONDecodeError as e:
        logger.error(f"Failed to parse LLM response as JSON: {e}")
        logger.error(f"Response text: {response_text}")
        
        fallback_java = response_text
        fallback_explanation = "LLM response could not be parsed as JSON. Raw output provided."
        
        if "public class" in response_text:
            class_start = response_text.find("public class")
            fallback_java = response_text[class_start:].strip()
            fallback_explanation = "Extracted Java class from raw LLM output."
        
        return {
            "java_code": fallback_java,
            "explanation": fallback_explanation
        }
    except Exception as e:
        logger.error(f"Error parsing response: {e}")
        return {
            "java_code": "// Error: Failed to parse translation response\n// Original error: " + str(e),
            "explanation": "Translation failed due to parsing error."
        }

def translate_cobol_to_java(cobol_code: str, model: str = "llama-3.1-8b-instant") -> Dict[str, str]:
    try:
        logger.info(f"Starting COBOL to Java translation using model: {model}")
        
        client = get_groq_client()
        
        prompt = get_translation_prompt(cobol_code)
        
        messages = [
            {
                "role": "system",
                "content": "You are an expert banking system architect specializing in COBOL to Java modernization."
            },
            {
                "role": "user",
                "content": prompt
            }
        ]
        
        logger.info("Sending request to Groq API")
        response = client.create_chat_completion(
            messages=messages,
            model=model,
            temperature=0.1,
            max_tokens=8192
        )
        
        if not response["success"]:
            error_msg = response.get("error", "Unknown error")
            raise Exception(f"Groq API call failed: {error_msg}")
        
        logger.info(f"Received response from Groq API. Tokens used: {response['usage']['total_tokens']}")
        
        result = parse_llm_response(response["content"])
        
        logger.info("Translation completed successfully")
        return result
        
    except Exception as e:
        logger.error(f"Translation failed: {str(e)}")
        return {
            "java_code": f"// Translation Error: {str(e)}\n// Original COBOL code could not be processed.",
            "explanation": f"Translation failed due to error: {str(e)}"
        }