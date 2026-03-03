/*
 * SWE 261P - Part 5: Testable Design & Mocking
 * Author: Zian Xu (zianx11@uci.edu)
 *
 * This test class demonstrates:
 * 1. Stubbing: A manual stub of UpdateService used to test update checking logic
 * 2. Bad Testable Design Fix: DefaultUpdateService's hardcoded URL creation
 * 3. Mocking: Mockito mock of StageService to verify StageServiceController
 */
package org.pdfsam.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.lifecycle.CleanupRequest;
import org.pdfsam.model.ui.SetLatestStageStatusRequest;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.service.ui.StageService;
import org.pdfsam.service.ui.StageServiceController;
import org.pdfsam.service.update.UpdateService;
import org.pdfsam.test.ClearEventStudioExtension;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

@ExtendWith(ClearEventStudioExtension.class)
public class ZianPart5Test {

    // ========================================================================
    // SECTION 1: STUBBING — Manual Stub of UpdateService
    // ========================================================================

    static class StubUpdateService implements UpdateService {
        private final String version;

        StubUpdateService(String version) {
            this.version = version;
        }

        @Override
        public String getLatestVersion() {
            return version;
        }
    }

    static class UpdateChecker {
        private final UpdateService updateService;
        private final String currentVersion;

        UpdateChecker(UpdateService updateService, String currentVersion) {
            this.updateService = updateService;
            this.currentVersion = currentVersion;
        }

        public boolean isUpdateAvailable() {
            String latest = updateService.getLatestVersion();
            return !latest.isEmpty() && !latest.equals(currentVersion);
        }

        public String getUpdateMessage() {
            String latest = updateService.getLatestVersion();
            if (latest.isEmpty()) {
                return "Unable to check for updates.";
            }
            if (latest.equals(currentVersion)) {
                return "You are using the latest version.";
            }
            return "Update available: " + latest;
        }
    }

    @Nested
    @DisplayName("Stubbing: StubUpdateService with UpdateChecker")
    class StubbingTest {

        @Test
        @DisplayName("Stub returning newer version — update available")
        void stubNewerVersion() {
            StubUpdateService stub = new StubUpdateService("6.0.0");
            UpdateChecker checker = new UpdateChecker(stub, "5.4.5");

            assertTrue(checker.isUpdateAvailable());
            assertEquals("Update available: 6.0.0", checker.getUpdateMessage());
        }

        @Test
        @DisplayName("Stub returning same version — no update available")
        void stubSameVersion() {
            StubUpdateService stub = new StubUpdateService("5.4.5");
            UpdateChecker checker = new UpdateChecker(stub, "5.4.5");

            assertFalse(checker.isUpdateAvailable());
            assertEquals("You are using the latest version.", checker.getUpdateMessage());
        }

        @Test
        @DisplayName("Stub returning empty string — unable to check")
        void stubEmptyVersion() {
            StubUpdateService stub = new StubUpdateService("");
            UpdateChecker checker = new UpdateChecker(stub, "5.4.5");

            assertFalse(checker.isUpdateAvailable());
            assertEquals("Unable to check for updates.", checker.getUpdateMessage());
        }
    }

    // ========================================================================
    // SECTION 2: BAD TESTABLE DESIGN — DefaultUpdateService hardcoded URL
    // ========================================================================

    interface VersionDataProvider {
        InputStream fetchVersionData() throws IOException;
    }

    static class TestableUpdateService implements UpdateService {
        private static final String CURRENT_VERSION_KEY = "currentVersion";
        private final VersionDataProvider dataProvider;
        private final com.fasterxml.jackson.databind.ObjectMapper objectMapper;

        TestableUpdateService(VersionDataProvider dataProvider,
                              com.fasterxml.jackson.databind.ObjectMapper objectMapper) {
            this.dataProvider = dataProvider;
            this.objectMapper = objectMapper;
        }

        @Override
        public String getLatestVersion() {
            try (InputStream is = dataProvider.fetchVersionData()) {
                java.util.Map<String, Object> map = objectMapper.readValue(is, java.util.Map.class);
                return map.getOrDefault(CURRENT_VERSION_KEY, "").toString();
            } catch (IOException e) {
                // Log and return empty
            }
            return "";
        }
    }

    @Nested
    @DisplayName("Bad Testable Design Fix: Injectable VersionDataProvider")
    class BadTestableDesignTest {

        private com.fasterxml.jackson.databind.ObjectMapper objectMapper;

        @BeforeEach
        void setUp() {
            objectMapper = new com.fasterxml.jackson.databind.ObjectMapper();
        }

        @Test
        @DisplayName("TestableUpdateService reads version from injected provider")
        void readVersionFromProvider() {
            // Provide JSON data directly without any URL or network access
            VersionDataProvider provider = () -> new java.io.ByteArrayInputStream(
                    "{\"currentVersion\":\"6.0.0\"}".getBytes());

            TestableUpdateService service = new TestableUpdateService(provider, objectMapper);

            assertEquals("6.0.0", service.getLatestVersion());
        }

        @Test
        @DisplayName("TestableUpdateService returns empty on IOException")
        void returnsEmptyOnError() {
            VersionDataProvider provider = () -> {
                throw new IOException("Network error");
            };

            TestableUpdateService service = new TestableUpdateService(provider, objectMapper);

            assertEquals("", service.getLatestVersion());
        }

        @Test
        @DisplayName("TestableUpdateService handles missing key")
        void handlesMissingKey() {
            VersionDataProvider provider = () -> new java.io.ByteArrayInputStream(
                    "{\"otherField\":\"value\"}".getBytes());

            TestableUpdateService service = new TestableUpdateService(provider, objectMapper);

            assertEquals("", service.getLatestVersion());
        }

        @Test
        @DisplayName("TestableUpdateService handles empty JSON")
        void handlesEmptyJson() {
            VersionDataProvider provider = () -> new java.io.ByteArrayInputStream("{}".getBytes());

            TestableUpdateService service = new TestableUpdateService(provider, objectMapper);

            assertEquals("", service.getLatestVersion());
        }
    }

    // ========================================================================
    // SECTION 3: MOCKING — Mockito mock of StageService
    // ========================================================================

    @Nested
    @DisplayName("Mocking: Verify StageServiceController interactions with Mockito")
    @ExtendWith(ClearEventStudioExtension.class)
    class MockingTest {

        private StageService mockService;
        private StageServiceController controller;

        @BeforeEach
        void setUp() {
            mockService = mock(StageService.class);
            controller = new StageServiceController(mockService);
        }

        @Test
        @DisplayName("requestStageStatus forwards event to service.save()")
        void requestForwardsToSave() {
            SetLatestStageStatusRequest event = new SetLatestStageStatusRequest(StageStatus.NULL);

            controller.requestStageStatus(event);

            verify(mockService).save(StageStatus.NULL);
        }

        @Test
        @DisplayName("requestStageStatus passes exact status to service")
        void requestPassesCorrectStatus() {
            StageStatus status = new StageStatus(100, 200, 800, 600);
            SetLatestStageStatusRequest event = new SetLatestStageStatusRequest(status);

            controller.requestStageStatus(event);

            verify(mockService).save(status);
            verifyNoMoreInteractions(mockService);
        }

        @Test
        @DisplayName("onCleanupRequest calls service.clear()")
        void cleanupCallsClear() {
            controller.onCleanupRequest(new CleanupRequest());

            verify(mockService).clear();
        }

        @Test
        @DisplayName("Broadcast CleanupRequest triggers service.clear() via event studio")
        void broadcastCleanupTriggersClear() {
            eventStudio().broadcast(new CleanupRequest());

            verify(mockService).clear();
        }

        @Test
        
        @DisplayName("Multiple status requests each forward to service.save()")
        void multipleRequests() {
            StageStatus status1 = StageStatus.NULL;
            StageStatus status2 = new StageStatus(0, 0, 1024, 768);

            controller.requestStageStatus(new SetLatestStageStatusRequest(status1));
            controller.requestStageStatus(new SetLatestStageStatusRequest(status2));

            verify(mockService).save(status1);
            verify(mockService).save(status2);
            verify(mockService, times(2)).save(any(StageStatus.class));
        }
    }
}
