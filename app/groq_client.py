import os
import json
from typing import Dict, Any, Optional
import logging
from groq import Groq, GroqError

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class GroqClient:
    def __init__(self):
        self.api_key = os.getenv("GROQ_API_KEY")
        if not self.api_key:
            raise ValueError("GROQ_API_KEY environment variable is not set")
        
        self.client = Groq(api_key=self.api_key)
        logger.info("Groq client initialized successfully")

    def create_chat_completion(
        self,
        messages: list,
        model: str = "llama-3.1-8b-instant",
        temperature: float = 0.1,
        max_tokens: int = 8192
    ) -> Dict[str, Any]:
        try:
            response = self.client.chat.completions.create(
                model=model,
                messages=messages,
                temperature=temperature,
                max_tokens=max_tokens
            )
            
            content = response.choices[0].message.content
            return {
                "success": True,
                "content": content,
                "model": model,
                "usage": {
                    "prompt_tokens": response.usage.prompt_tokens,
                    "completion_tokens": response.usage.completion_tokens,
                    "total_tokens": response.usage.total_tokens
                }
            }
            
        except GroqError as e:
            logger.error(f"Groq API error: {str(e)}")
            return {
                "success": False,
                "error": f"Groq API error: {str(e)}",
                "content": None
            }
        except Exception as e:
            logger.error(f"Unexpected error: {str(e)}")
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "content": None
            }

def get_groq_client() -> GroqClient:
    return GroqClient()