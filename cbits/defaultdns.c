#include <winsock2.h>
#include <iphlpapi.h>
#include <stdio.h>
#include <string.h>
#include <windows.h>
#include <iostream>

#define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define FREE(x) HeapFree(GetProcessHeap(), 0, (x))

typedef struct {
    DWORD error;
    char dnsAddresses[255];
} dns_t;

dns_t getWindowsDefDnsServers(void) {
    FIXED_INFO *pFixedInfo;
    ULONG ulOutBufLen;
    DWORD dwRetVal;
    dns_t result;

    result.error = NO_ERROR;
    pFixedInfo = (FIXED_INFO *) MALLOC(sizeof (FIXED_INFO));
    if (pFixedInfo == NULL) {
      result.error = ERROR_NOT_ENOUGH_MEMORY;
      return result;
    }
    ulOutBufLen = sizeof (FIXED_INFO);

    // Make an initial call to GetAdaptersInfo to get the necessary size into the
    // ulOutBufLen variable
    if (GetNetworkParams(pFixedInfo, &ulOutBufLen) == ERROR_BUFFER_OVERFLOW) {
        FREE(pFixedInfo);
        pFixedInfo = (FIXED_INFO *) MALLOC(ulOutBufLen);
        if (pFixedInfo == NULL) {
            result.error = ERROR_NOT_ENOUGH_MEMORY;
            return result;
        }
    }

    dwRetVal = GetNetworkParams(pFixedInfo, &ulOutBufLen);

    if (dwRetVal == NO_ERROR) {
      int offset = 0;
      int space_available = 255;
      IP_ADDR_STRING* head = &pFixedInfo->DnsServerList;
      while (head != NULL && space_available >= 17) {
        strcpy_s(&result.dnsAddresses[offset], 15, head->IpAddress.String);
        // Update the offset (non-null-terminated IP address + separator)
        offset += 16;
        // Update the space available, pessimistically
        space_available -= 16;
        // Write the separator, but only if this is not the last one,
        // otherwise terminate the string.
        head = head->Next;
        if (head == NULL)
          result.dnsAddresses[offset] = '\0';
        else
          result.dnsAddresses[offset] = ',';
      }
    }

    else {
        if (pFixedInfo) FREE(pFixedInfo);
        result.error = dwRetVal;
    }

    if (pFixedInfo) FREE(pFixedInfo);
    return result;
}
