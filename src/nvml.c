/*
Copyright (c) 2025 Mohammad Amin Zadenoori
Copyright (c) 2025 Gabriele Sales
*/

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#ifdef HAVE_NVML
#include <nvml.h>
#endif

/* SUPPORT NVML */
#ifdef HAVE_NVML

/* SUPPORT MULTIPLE NVML HEADERS */
#ifndef NVML_GPU_INSTANCE_ID_NONE
#ifdef INVALID_GPU_INSTANCE_ID
#define NVML_GPU_INSTANCE_ID_NONE INVALID_GPU_INSTANCE_ID
#else
#define NVML_GPU_INSTANCE_ID_NONE 0xFFFFFFFFU
#endif
#endif

#ifndef NVML_COMPUTE_INSTANCE_ID_NONE
#ifdef INVALID_COMPUTE_INSTANCE_ID
#define NVML_COMPUTE_INSTANCE_ID_NONE INVALID_COMPUTE_INSTANCE_ID
#else
#define NVML_COMPUTE_INSTANCE_ID_NONE 0xFFFFFFFFU
#endif
#endif

/* HELP FUNCTIONS */
static int nvml_is_initialized = 0;

static nvmlReturn_t nvml_ensure_init(void) {
    nvmlReturn_t result;

    if (nvml_is_initialized) {
        return NVML_SUCCESS;
    }

    result = nvmlInit_v2();
    if (result == NVML_SUCCESS) {
        nvml_is_initialized = 1;
    }

    return result;
}

static void nvml_cleanup(void) {
    if (!nvml_is_initialized) {
        return;
    }

    if (nvmlShutdown() == NVML_SUCCESS) {
        nvml_is_initialized = 0;
    }
}

static nvmlReturn_t nvml_get_device_count(unsigned int *count) {
    nvmlReturn_t result = nvml_ensure_init();
    if (result != NVML_SUCCESS) {
        return result;
    }

    return nvmlDeviceGetCount(count);
}

static nvmlReturn_t nvml_get_device(int device_index, nvmlDevice_t *device) {
    unsigned int count = 0;
    nvmlReturn_t result = nvml_get_device_count(&count);
    if (result != NVML_SUCCESS) {
        return result;
    }

    if (device_index < 0 || (unsigned int) device_index >= count) {
        return NVML_ERROR_INVALID_ARGUMENT;
    }

    return nvmlDeviceGetHandleByIndex((unsigned int) device_index, device);
}
#endif

static SEXP nvml_alloc_process_info(R_xlen_t n) {
    SEXP data = PROTECT(allocVector(VECSXP, 5));
    SET_VECTOR_ELT(data, 0, allocVector(INTSXP, n));
    SET_VECTOR_ELT(data, 1, allocVector(INTSXP, n));
    SET_VECTOR_ELT(data, 2, allocVector(REALSXP, n));
    SET_VECTOR_ELT(data, 3, allocVector(INTSXP, n));
    SET_VECTOR_ELT(data, 4, allocVector(INTSXP, n));
    UNPROTECT(1);
    return data;
}

static SEXP nvml_alloc_metrics(void) {
    SEXP data = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(data, 0, ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(data, 1, ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(data, 2, ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(data, 3, ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(data, 4, ScalarReal(NA_REAL));
    SET_VECTOR_ELT(data, 5, ScalarReal(NA_REAL));
    UNPROTECT(1);
    return data;
}


/* API */
#ifdef HAVE_NVML
SEXP nvml_is_available_c(void) {
    unsigned int count = 0;
    nvmlReturn_t result = nvml_get_device_count(&count);
    return ScalarLogical(result == NVML_SUCCESS);
}

SEXP nvml_device_count_c(void) {
    unsigned int count = 0;
    nvmlReturn_t result = nvml_get_device_count(&count);
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }
    return ScalarInteger((int)count);
}

SEXP nvml_device_info_c(SEXP device_index_sexp) {
    int device_index = asInteger(device_index_sexp);
    nvmlDevice_t device;
    nvmlReturn_t result = nvml_get_device(device_index, &device);
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }

    char name[NVML_DEVICE_NAME_BUFFER_SIZE] = {0};
    char uuid[NVML_DEVICE_UUID_BUFFER_SIZE] = {0};
    nvmlMemory_t memory_info = {0};

    result = nvmlDeviceGetName(device, name, sizeof(name));
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }

    result = nvmlDeviceGetUUID(device, uuid, sizeof(uuid));
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }

    result = nvmlDeviceGetMemoryInfo(device, &memory_info);
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }

    SEXP data = PROTECT(allocVector(VECSXP, 4));
    SET_VECTOR_ELT(data, 0, ScalarInteger(device_index));
    SET_VECTOR_ELT(data, 1, mkString(name));
    SET_VECTOR_ELT(data, 2, mkString(uuid));
    SET_VECTOR_ELT(data, 3, ScalarReal((double) memory_info.total));
    UNPROTECT(1);
    return data;
}

SEXP nvml_device_compute_processes_c(SEXP device_index_sexp) {
    int device_index = asInteger(device_index_sexp);
    nvmlDevice_t device;
    nvmlReturn_t result = nvml_get_device(device_index, &device);
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }

    unsigned int info_count = 0;
    /* First call asks NVML how many process records are currently available. */
    result = nvmlDeviceGetComputeRunningProcesses(device, &info_count, NULL);
    if (result == NVML_ERROR_NOT_SUPPORTED) {
        return nvml_alloc_process_info(0);
    }
    if (result != NVML_SUCCESS && result != NVML_ERROR_INSUFFICIENT_SIZE) {
        return ScalarInteger(-(int) result);
    }
    if (info_count == 0) {
        return nvml_alloc_process_info(0);
    }

    nvmlProcessInfo_t *infos = (nvmlProcessInfo_t *) R_Calloc(info_count, nvmlProcessInfo_t);
    /* Second call fills the caller-allocated buffer returned by the size probe above. */
    result = nvmlDeviceGetComputeRunningProcesses(device, &info_count, infos);
    if (result == NVML_ERROR_NOT_SUPPORTED) {
        R_Free(infos);
        return nvml_alloc_process_info(0);
    }
    if (result != NVML_SUCCESS) {
        R_Free(infos);
        return ScalarInteger(-(int) result);
    }

    SEXP data = PROTECT(nvml_alloc_process_info((R_xlen_t) info_count));
    int *device_col = INTEGER(VECTOR_ELT(data, 0));
    int *pid_col = INTEGER(VECTOR_ELT(data, 1));
    double *used_mem_col = REAL(VECTOR_ELT(data, 2));
    int *gpu_instance_col = INTEGER(VECTOR_ELT(data, 3));
    int *compute_instance_col = INTEGER(VECTOR_ELT(data, 4));

    for (unsigned int i = 0; i < info_count; ++i) {
        device_col[i] = device_index;
        pid_col[i] = (int) infos[i].pid;
        used_mem_col[i] = infos[i].usedGpuMemory == NVML_VALUE_NOT_AVAILABLE ?
            NA_REAL : (double) infos[i].usedGpuMemory;
        gpu_instance_col[i] = infos[i].gpuInstanceId == NVML_GPU_INSTANCE_ID_NONE ?
            NA_INTEGER : (int) infos[i].gpuInstanceId;
        compute_instance_col[i] = infos[i].computeInstanceId == NVML_COMPUTE_INSTANCE_ID_NONE ?
            NA_INTEGER : (int) infos[i].computeInstanceId;
    }

    R_Free(infos);
    UNPROTECT(1);
    return data;
}

SEXP nvml_get_metrics_c(SEXP device_index_sexp) {
    int device_index = asInteger(device_index_sexp);
    nvmlDevice_t device;
    nvmlReturn_t result = nvml_get_device(device_index, &device);
    if (result != NVML_SUCCESS) {
        return ScalarInteger(-(int) result);
    }

    nvmlUtilization_t utilization = {NA_INTEGER, NA_INTEGER};
    unsigned int temp = NA_INTEGER;
    unsigned int power = NA_INTEGER;
    nvmlMemory_t memory_info = {0};
    double memory_used_bytes = NA_REAL;
    double memory_total_device_bytes = NA_REAL;

    result = nvmlDeviceGetUtilizationRates(device, &utilization);
    if (result != NVML_SUCCESS) {
        utilization.gpu = NA_INTEGER;
        utilization.memory = NA_INTEGER;
    }

    nvmlTemperature_t temperature = {0};
    temperature.version = nvmlTemperature_v1;
    temperature.sensorType = NVML_TEMPERATURE_GPU;

    result = nvmlDeviceGetTemperatureV(device, &temperature);
    if (result == NVML_SUCCESS && temperature.temperature >= 0) {
        temp = (unsigned int) temperature.temperature;
    } else {
        temp = NA_INTEGER;
    }

    result = nvmlDeviceGetPowerUsage(device, &power);
    if (result != NVML_SUCCESS) {
        power = NA_INTEGER;
    }

    result = nvmlDeviceGetMemoryInfo(device, &memory_info);
    if (result == NVML_SUCCESS) {
        memory_used_bytes = (double) memory_info.used;
        memory_total_device_bytes = (double) memory_info.total;
    }

    SEXP data = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(data, 0, ScalarInteger((int) utilization.gpu));
    SET_VECTOR_ELT(data, 1, ScalarInteger((int) utilization.memory));
    SET_VECTOR_ELT(data, 2, ScalarInteger((int) temp));
    SET_VECTOR_ELT(data, 3, ScalarInteger((int) power));
    SET_VECTOR_ELT(data, 4, ScalarReal(memory_used_bytes));
    SET_VECTOR_ELT(data, 5, ScalarReal(memory_total_device_bytes));
    UNPROTECT(1);
    return data;
}

SEXP nvml_error_string_c(SEXP err_code_sexp) {
    if (!isInteger(err_code_sexp) && !isReal(err_code_sexp)) {
        Rf_error("nvml_error_string_c: err_code must be an integer");
    }
    int err_code = asInteger(err_code_sexp);
    const char *msg = nvmlErrorString((nvmlReturn_t) err_code);
    if (msg == NULL) {
        msg = "Unknown NVML error";
    }
    return mkString(msg);
}
#else
SEXP nvml_is_available_c(void) {
    return ScalarLogical(0);
}

SEXP nvml_device_count_c(void) {
    return ScalarInteger(0);
}

SEXP nvml_device_info_c(SEXP device_index_sexp) {
    (void) device_index_sexp;
    Rf_error("NVML support is not available in this build");
    return R_NilValue;
}

SEXP nvml_device_compute_processes_c(SEXP device_index_sexp) {
    (void) device_index_sexp;
    return nvml_alloc_process_info(0);
}

SEXP nvml_get_metrics_c(SEXP device_index_sexp) {
    (void) device_index_sexp;
    return nvml_alloc_metrics();
}

SEXP nvml_error_string_c(SEXP err_code_sexp) {
    (void) err_code_sexp;
    return mkString("NVML support is not available in this build");
}
#endif

static const R_CallMethodDef callMethods[] = {
    {"nvml_is_available_c", (DL_FUNC) &nvml_is_available_c, 0},
    {"nvml_device_count_c",  (DL_FUNC) &nvml_device_count_c,  0},
    {"nvml_device_info_c",   (DL_FUNC) &nvml_device_info_c,   1},
    {"nvml_device_compute_processes_c", (DL_FUNC) &nvml_device_compute_processes_c, 1},
    {"nvml_get_metrics_c",   (DL_FUNC) &nvml_get_metrics_c,   1},
    {"nvml_error_string_c",  (DL_FUNC) &nvml_error_string_c,  1},
    {NULL, NULL, 0}
};

void R_init_CudaMon(DllInfo *dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

void R_unload_CudaMon(DllInfo *dll) {
    (void) dll;
#ifdef HAVE_NVML
    nvml_cleanup();
#endif
}
