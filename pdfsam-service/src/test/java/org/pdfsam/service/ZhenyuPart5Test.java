package org.pdfsam.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.service.task.TaskExecutionController;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.mockito.ArgumentCaptor;
import org.sejda.core.service.TaskExecutionService;
import org.sejda.model.parameter.base.AbstractParameters;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(ClearEventStudioExtension.class)
public class ZhenyuPart5Test {

    // SECTION 1: STUBBING — Manual Stub of UsageService

    /**
     * Unlike a Mockito mock, this is a real class that implements the interface.
     * Instead of persisting data to Java Preferences, it records calls in memory
     * so tests can verify state without a database or I/O.
     */
    static class StubUsageService implements UsageService {
        private final List<String> incrementedModules = new ArrayList<>();
        private long totalUsages = 0;

        @Override
        public void incrementUsageFor(String moduleId) {
            incrementedModules.add(moduleId);
            totalUsages++;
        }

        @Override
        public void clear() {
            incrementedModules.clear();
            totalUsages = 0;
        }

        @Override
        public long getTotalUsages() {
            return totalUsages;
        }

        // Test helper
        public List<String> getIncrementedModules() {
            return incrementedModules;
        }
    }

    @Nested
    @DisplayName("StubbingTest")
    @ExtendWith(ClearEventStudioExtension.class)
    class StubbingTest {

        private StubUsageService stubUsageService;
        private TaskExecutionService mockExecutionService;
        private TaskExecutionController controller;

        @BeforeEach
        void setUp() {
            stubUsageService = new StubUsageService();
            mockExecutionService = mock(TaskExecutionService.class);
            controller = new TaskExecutionController(mockExecutionService, stubUsageService);
        }

        @Test
        @DisplayName("Stub records basic usage increment")
        void stubRecordsUsageIncrement() {
            AbstractParameters params = mock(AbstractParameters.class);
            TaskExecutionRequest request = new TaskExecutionRequest("merge", params);

            controller.request(request);

            assertEquals(1, stubUsageService.getTotalUsages());
            assertEquals(1, stubUsageService.getIncrementedModules().size());
            assertEquals("merge", stubUsageService.getIncrementedModules().get(0));
        }

        @Test
        @DisplayName("Stub records multiple usage increments")
        void stubRecordsMultipleIncrements() {
            AbstractParameters params = mock(AbstractParameters.class);

            controller.request(new TaskExecutionRequest("merge", params));
            controller.request(new TaskExecutionRequest("split", params));
            controller.request(new TaskExecutionRequest("merge", params));

            assertEquals(3, stubUsageService.getTotalUsages());
            assertEquals(List.of("merge", "split", "merge"), stubUsageService.getIncrementedModules());
        }

        @Test
        @DisplayName("Stub clear resets all counters")
        void stubClearResets() {
            AbstractParameters params = mock(AbstractParameters.class);
            controller.request(new TaskExecutionRequest("merge", params));

            stubUsageService.clear();

            assertEquals(0, stubUsageService.getTotalUsages());
            assertTrue(stubUsageService.getIncrementedModules().isEmpty());
        }
    }

    // SECTION 2: BAD TESTABLE DESIGN — DefaultPdfLoadService.fxMoveStatusTo()

    /**
     * This is bad testable design because:
     * 1. It is a STATIC method — cannot be overridden or stubbed.
     * 2. It calls Platform.runLater() — requires a running JavaFX runtime.
     * 3. It is PRIVATE — cannot be accessed or replaced by tests.
     *
     * This makes it impossible to unit-test the PDF loading logic without
     * starting the entire JavaFX application thread.
     *
     * FIX: Extract status updating into an injectable interface.
     */

    interface StatusUpdater {
        void moveStatusTo(PdfDocumentDescriptor descriptor, PdfDescriptorLoadingStatus status);
    }

    // Testable implementation that updates status directly, no JavaFX needed.
    static class DirectStatusUpdater implements StatusUpdater {        
        private final List<String> statusLog = new ArrayList<>();

        @Override
        public void moveStatusTo(PdfDocumentDescriptor descriptor, PdfDescriptorLoadingStatus status) {
            // Directly update without Platform.runLater()
            descriptor.moveStatusTo(status);
            statusLog.add(descriptor.getFileName() + " -> " + status.name());
        }

        // Helper method to get the status log
        public List<String> getStatusLog() {
            return statusLog;
        }
    }

    /**
     * A testable version of loading logic that uses the injectable StatusUpdater instead of a hardcoded static Platform.runLater() call.
     *
     * Covers all 4 branches from the original code:
     * 1. LOADING → LOADED (no password, successful)
     * 2. LOADING → LOADED_WITH_USER_PWD_DECRYPTION (has password, successful)
     * 3. LOADING → ENCRYPTED (InvalidPasswordException)
     * 4. LOADING → WITH_ERRORS (general Exception)
     */
    static class TestablePdfLoadLogic {
        private final StatusUpdater statusUpdater;
        private final PdfParserFunction parserFunction;

        TestablePdfLoadLogic(StatusUpdater statusUpdater, PdfParserFunction parserFunction) {
            this.statusUpdater = statusUpdater;
            this.parserFunction = parserFunction;
        }

        TestablePdfLoadLogic(StatusUpdater statusUpdater) {
            this(statusUpdater, descriptor -> {});
        }

        public void loadDescriptor(PdfDocumentDescriptor descriptor) {
            statusUpdater.moveStatusTo(descriptor, PdfDescriptorLoadingStatus.LOADING);
            try {
                // Delegate to injectable parser
                parserFunction.parse(descriptor);

                if (descriptor.hasPassword()) {
                    statusUpdater.moveStatusTo(descriptor, PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION);
                } else {
                    statusUpdater.moveStatusTo(descriptor, PdfDescriptorLoadingStatus.LOADED);
                }
            } catch (InvalidPasswordException e) {
                statusUpdater.moveStatusTo(descriptor, PdfDescriptorLoadingStatus.ENCRYPTED);
            } catch (Exception e) {
                statusUpdater.moveStatusTo(descriptor, PdfDescriptorLoadingStatus.WITH_ERRORS);
            }
        }
    }

    @FunctionalInterface
    interface PdfParserFunction {
        void parse(PdfDocumentDescriptor descriptor) throws Exception;
    }

    static class InvalidPasswordException extends Exception {
        InvalidPasswordException(String msg) {
            super(msg);
        }
    }

    @Nested
    @DisplayName("Bad Testable Design Fix")
    class BadTestableDesignTest {

        private DirectStatusUpdater statusUpdater;
        private TestablePdfLoadLogic loadLogic;

        @BeforeEach
        void setUp() {
            statusUpdater = new DirectStatusUpdater();
            loadLogic = new TestablePdfLoadLogic(statusUpdater);
        }

        @Test
        @DisplayName("Successful load without password transitions LOADING → LOADED")
        void loadWithoutPassword() {
            File tempFile = new File("test.pdf");
            PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(tempFile);
            descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);

            loadLogic.loadDescriptor(descriptor);

            assertEquals(PdfDescriptorLoadingStatus.LOADED, descriptor.loadingStatus().getValue());
        }

        @Test
        @DisplayName("Successful load with password transitions LOADING → LOADED_WITH_USER_PWD_DECRYPTION")
        void loadWithPassword() {
            File tempFile = new File("encrypted.pdf");
            PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptor(tempFile, "secret");
            descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);

            loadLogic.loadDescriptor(descriptor);

            assertEquals(PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION, descriptor.loadingStatus().getValue());
        }

        @Test
        @DisplayName("Invalid password transitions LOADING → ENCRYPTED")
        void loadWithInvalidPassword() {
            File tempFile = new File("locked.pdf");
            PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(tempFile);
            descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);

            // Inject a parser that throws InvalidPasswordException
            TestablePdfLoadLogic errorLogic = new TestablePdfLoadLogic(statusUpdater,
                    d -> {
                        throw new InvalidPasswordException("Bad password");
                    });

            errorLogic.loadDescriptor(descriptor);

            assertEquals(PdfDescriptorLoadingStatus.ENCRYPTED, descriptor.loadingStatus().getValue());
        }

        @Test
        @DisplayName("General exception during load transitions LOADING → WITH_ERRORS")
        void loadWithGeneralError() {
            File tempFile = new File("corrupted.pdf");
            PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(tempFile);
            descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);

            // Inject a parser that throws a general exception
            TestablePdfLoadLogic errorLogic = new TestablePdfLoadLogic(statusUpdater,
                    d -> {
                        throw new RuntimeException("Corrupt PDF");
                    });

            errorLogic.loadDescriptor(descriptor);

            assertEquals(PdfDescriptorLoadingStatus.WITH_ERRORS, descriptor.loadingStatus().getValue());
        }

        @Test
        @DisplayName("StatusUpdater logs all transitions correctly")
        void statusUpdaterLogs() {
            File tempFile = new File("test.pdf");
            PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(tempFile);
            descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);

            loadLogic.loadDescriptor(descriptor);

            assertEquals(2, statusUpdater.getStatusLog().size());
            assertTrue(statusUpdater.getStatusLog().get(0).contains("LOADING"));
            assertTrue(statusUpdater.getStatusLog().get(1).contains("LOADED"));
        }
    }

    // SECTION 3: MOCKING — Mockito mock of TaskExecutionService

    @Nested
    @DisplayName("MockingTest")
    @ExtendWith(ClearEventStudioExtension.class)
    class MockingTest {

        private TaskExecutionService mockExecutionService;
        private UsageService mockUsageService;
        private TaskExecutionController controller;

        @BeforeEach
        void setUp() {
            mockExecutionService = mock(TaskExecutionService.class);
            mockUsageService = mock(UsageService.class);
            controller = new TaskExecutionController(mockExecutionService, mockUsageService);
        }

        @Test
        @DisplayName("request() delegates to executionService.execute() with correct parameters")
        void requestDelegatesToExecutionService() {
            AbstractParameters params = mock(AbstractParameters.class);
            TaskExecutionRequest request = new TaskExecutionRequest("merge", params);

            controller.request(request);

            verify(mockExecutionService, timeout(1000)).execute(params);
        }

        @Test
        @DisplayName("request() passes different parameters to execute() for each request")
        void requestPassesDifferentParams() {
            AbstractParameters params1 = mock(AbstractParameters.class);
            AbstractParameters params2 = mock(AbstractParameters.class);

            controller.request(new TaskExecutionRequest("merge", params1));
            controller.request(new TaskExecutionRequest("split", params2));

            // ArgumentCaptor captures actual arguments passed to execute()
            ArgumentCaptor<AbstractParameters> captor = ArgumentCaptor.forClass(AbstractParameters.class);
            verify(mockExecutionService, timeout(1000).times(2)).execute(captor.capture());

            List<AbstractParameters> capturedParams = captor.getAllValues();
            assertEquals(2, capturedParams.size());
            assertSame(params1, capturedParams.get(0));
            assertSame(params2, capturedParams.get(1));
        }

        // NOTE: This test intentionally causes a RuntimeException inside the executor thread.
        // The exception stack trace printed to stderr is EXPECTED and does not indicate a test failure.
        @Test
        @DisplayName("execute() throwing exception does not prevent subsequent requests")
        void executionServiceExceptionDoesNotBlock() {
            AbstractParameters failParams = mock(AbstractParameters.class);
            AbstractParameters successParams = mock(AbstractParameters.class);

            // Configure first call to throw, simulating a task failure
            doThrow(new RuntimeException("Task failed")).when(mockExecutionService).execute(failParams);

            controller.request(new TaskExecutionRequest("merge", failParams));
            controller.request(new TaskExecutionRequest("split", successParams));

            // Both calls should still be attempted (single-thread executor runs sequentially)
            verify(mockExecutionService, timeout(1000)).execute(failParams);
            verify(mockExecutionService, timeout(1000)).execute(successParams);
        }

        @Test
        @DisplayName("request() calls execute() exactly once per request, no extra invocations")
        void executeCalledOncePerRequest() {
            AbstractParameters params = mock(AbstractParameters.class);

            controller.request(new TaskExecutionRequest("rotate", params));

            verify(mockExecutionService, timeout(1000).times(1)).execute(params);
            verifyNoMoreInteractions(mockExecutionService);
        }
    }
}
