# COBOL-to-Java Translation – Generative AI Proof of Concept

## 1. Overview

This repository contains a **Generative AI–based Proof of Concept (PoC)** designed to demonstrate the feasibility of **modernizing legacy COBOL applications into Java**.  
The solution follows **IBM Consulting enterprise architecture standards** and is implemented as a modular, API-driven system using **FastAPI** and **Uvicorn** as the ASGI application server.

The PoC focuses on code translation, refactoring, and documentation generation using Large Language Models (LLMs).

---

## 2. Objectives

The key objectives of this PoC are to:

- Validate the use of Generative AI for COBOL-to-Java translation
- Demonstrate clean, refactored, and documented Java output
- Implement a scalable REST-based backend using FastAPI and Uvicorn
- Showcase enterprise-aligned architecture and documentation
- Provide a foundation for future legacy modernization initiatives

---

## 3. Scope

### In Scope
- COBOL source code ingestion
- REST API–based translation workflow
- Prompt-driven LLM orchestration
- Structured Java code output
- Architecture, design, and lessons-learned documentation

### Out of Scope
- Production-grade security hardening
- Automated functional testing of translated code
- Performance benchmarking of generated applications
- End-to-end deployment in production environments

---

## 4. High-Level Architecture

The solution is implemented using a **layered architecture** that ensures separation of concerns and enterprise scalability.

![Solution Architecture](./Image/architecture.png)

### Architecture Layers

#### Client Layer
- Web browser–based UI
- Built using HTML, CSS, and JavaScript
- Allows users to submit COBOL code and view translated Java output

#### API Layer (FastAPI + Uvicorn)
- FastAPI application served using **Uvicorn ASGI server**
- Exposes REST endpoints for translation and monitoring
- Handles request validation, error handling, and orchestration

#### Translation Engine Layer
- Translator Module for workflow coordination
- Prompts Module for controlled LLM behavior
- Groq Client Module for external AI communication
- LLM Response Parser for structured output extraction

#### External AI Layer
- Groq Cloud Services
- High-performance LLM inference
- Models such as LLaMA 3 and Mixtral

---

## 5. Technology Stack

| Layer | Technology |
|------|-----------|
| Frontend | HTML, CSS, JavaScript |
| Backend API | FastAPI |
| ASGI Server | Uvicorn |
| Language | Python |
| AI Platform | Groq Cloud |
| LLM Models | LLaMA 3, Mixtral |
| Documentation | Markdown |

---

## 6. Application Execution

The backend application is implemented as a FastAPI service and executed using **Uvicorn**, which acts as the ASGI server.

### Sample Startup Command

```bash
uvicorn app.main:app --host 0.0.0.0 --port 8000 --reload
```
### Uvicorn Capabilities

Uvicorn enables:
- High-performance asynchronous request handling
- Compatibility with modern Python async frameworks
- Scalability for enterprise workloads

---

## 7. API Endpoints

| Endpoint | Method | Description |
|--------|--------|------------|
| `/translate` | POST | Submits COBOL code for translation |
| `/sample` | GET | Returns sample COBOL programs |
| `/health` | GET | Health check endpoint |

---

## 8. Translation Workflow

1. User submits COBOL source code from the client interface  
2. Request is sent securely to the FastAPI backend  
3. FastAPI application (running on Uvicorn) validates the request  
4. Translation Engine preprocesses the input  
5. Optimized prompts are sent to the LLM via Groq API  
6. LLM generates Java code and documentation  
7. Response Parser extracts structured output  
8. Final Java code is returned to the client  

---

## 9. Output Artifacts

The PoC produces:
- Clean, compilable Java source code
- Inline comments and documentation
- Logical mapping from COBOL constructs to Java equivalents

---

## 10. Key Design Considerations

- Layered architecture for maintainability
- Stateless REST APIs for scalability
- Explicit LLM response parsing for reliability
- Secure HTTPS-based communication
- Modular design for future extensibility

---

## 11. Risks and Limitations

- LLM outputs are non-deterministic
- Large COBOL programs may require segmentation
- Domain-specific business rules may need manual refinement
- Human validation remains mandatory

---

## 12. Future Enhancements

- Support for additional legacy languages
- Multi-LLM provider support
- Automated testing of generated Java code
- CI/CD pipeline integration
- Enterprise authentication and authorization

---

## 13. Conclusion

This Proof of Concept demonstrates the potential of **Generative AI** to accelerate legacy application modernization.  
By leveraging **FastAPI**, **Uvicorn**, and external LLM services, the solution provides an enterprise-aligned foundation suitable for further production hardening and large-scale adoption.
