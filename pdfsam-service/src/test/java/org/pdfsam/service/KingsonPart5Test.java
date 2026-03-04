/*
 * SWE 261P - Part 5: Testable Design & Mocking
 * Author: Kingson Zhang (kxzhang@uci.edu)
 *
 * This test class demonstrates:
 * 1. Stubbing: A manual stub of NewsService used to test a news consumer
 * 2. Bad Testable Design Fix: TaskExecutionController's hardcoded ExecutorService
 * 3. Mocking: Mockito mock of PreferencesRepository to verify DefaultRecentWorkspacesService
 */
package org.pdfsam.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.news.NewsData;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.service.news.NewsService;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.core.service.TaskExecutionService;
import org.sejda.model.parameter.base.AbstractParameters;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Part 5 test class for Kingson Zhang.
 */
@ExtendWith(ClearEventStudioExtension.class)
public class KingsonPart5Test {

    // ========================================================================
    // SECTION 1: STUBBING — Manual Stub of NewsService
    // ========================================================================

    /**
     * A manual stub implementation of NewsService.
     * Returns hardcoded news data without network access.
     */
    static class StubNewsService implements NewsService {
        private final List<NewsData> stubbedNews;
        private int latestNewsSeen = -1;
        private int latestImportantNewsSeen = -1;

        StubNewsService(List<NewsData> news) {
            this.stubbedNews = news;
        }

        @Override
        public List<NewsData> getLatestNews() {
            return stubbedNews;
        }

        @Override
        public int getLatestNewsSeen() {
            return latestNewsSeen;
        }

        @Override
        public void setLatestNewsSeen(int id) {
            this.latestNewsSeen = id;
        }

        @Override
        public int getLatestImportantNewsSeen() {
            return latestImportantNewsSeen;
        }

        @Override
        public void setLatestImportantNewsSeen(int id) {
            this.latestImportantNewsSeen = id;
        }

        @Override
        public void clear() {
            latestNewsSeen = -1;
            latestImportantNewsSeen = -1;
        }
    }

    /**
     * A simple consumer of NewsService that checks for unseen important news.
     * This simulates how a UI controller might consume the service.
     */
    static class NewsChecker {
        private final NewsService newsService;

        NewsChecker(NewsService newsService) {
            this.newsService = newsService;
        }

        /** Returns true if there are important news items the user hasn't seen. */
        public boolean hasUnseenImportantNews() {
            int lastSeen = newsService.getLatestImportantNewsSeen();
            return newsService.getLatestNews().stream()
                    .anyMatch(n -> n.important() && n.id() > lastSeen);
        }

        /** Returns the count of all available news items. */
        public int getNewsCount() {
            return newsService.getLatestNews().size();
        }

        /** Marks all news as seen by setting the ID of the latest one. */
        public void markAllSeen() {
            var news = newsService.getLatestNews();
            if (!news.isEmpty()) {
                int maxId = news.stream().mapToInt(NewsData::id).max().orElse(-1);
                newsService.setLatestNewsSeen(maxId);
                newsService.setLatestImportantNewsSeen(maxId);
            }
        }
    }

    @Nested
    @DisplayName("Stubbing: StubNewsService with NewsChecker")
    class StubbingTest {

        @Test
        @DisplayName("Stub returns hardcoded news — hasUnseenImportantNews returns true")
        void stubReturnsUnseenImportantNews() {
            List<NewsData> news = List.of(
                    new NewsData(1, "Title 1", "Content 1", LocalDate.now(), "http://link1.com", true),
                    new NewsData(2, "Title 2", "Content 2", LocalDate.now(), "http://link2.com", false));
            StubNewsService stub = new StubNewsService(news);
            NewsChecker checker = new NewsChecker(stub);

            assertTrue(checker.hasUnseenImportantNews());
            assertEquals(2, checker.getNewsCount());
        }

        @Test
        @DisplayName("After markAllSeen, hasUnseenImportantNews returns false")
        void stubAfterMarkAllSeen() {
            List<NewsData> news = List.of(
                    new NewsData(10, "Important", "Content", LocalDate.now(), "http://link.com", true));
            StubNewsService stub = new StubNewsService(news);
            NewsChecker checker = new NewsChecker(stub);

            checker.markAllSeen();

            assertFalse(checker.hasUnseenImportantNews());
            assertEquals(10, stub.getLatestNewsSeen());
            assertEquals(10, stub.getLatestImportantNewsSeen());
        }

        @Test
        @DisplayName("Empty stub returns no news")
        void stubEmptyNews() {
            StubNewsService stub = new StubNewsService(Collections.emptyList());
            NewsChecker checker = new NewsChecker(stub);

            assertFalse(checker.hasUnseenImportantNews());
            assertEquals(0, checker.getNewsCount());
        }
    }

    // ========================================================================
    // SECTION 2: BAD TESTABLE DESIGN — TaskExecutionController hardcoded
    // ExecutorService
    // ========================================================================

    /**
     * PROBLEM: In TaskExecutionController, the ExecutorService is hardcoded:
     * private final ExecutorService executor = Executors.newSingleThreadExecutor();
     *
     * This is bad testable design because:
     * 1. The ExecutorService is created internally — cannot be replaced for
     * testing.
     * 2. Tests cannot control when tasks execute (asynchronous, non-deterministic).
     * 3. Tests cannot verify task submission without actually executing tasks.
     * 4. Cannot use a synchronous executor to make tests deterministic.
     *
     * FIX: Accept ExecutorService as a constructor parameter (dependency
     * injection).
     */

    /**
     * A testable version of TaskExecutionController that accepts an injected
     * ExecutorService.
     */
    static class TestableTaskExecutionController {
        private final TaskExecutionService executionService;
        private final UsageService usageService;
        private final ExecutorService executor;
        private String currentModule = "";

        /** Constructor with injectable ExecutorService for testability. */
        TestableTaskExecutionController(TaskExecutionService executionService,
                UsageService usageService,
                ExecutorService executor) {
            this.executionService = executionService;
            this.usageService = usageService;
            this.executor = executor;
        }

        public void request(TaskExecutionRequest event) {
            usageService.incrementUsageFor(event.toolId());
            currentModule = event.toolId();
            executor.execute(() -> executionService.execute(event.parameters()));
        }

        public String getCurrentModule() {
            return currentModule;
        }

        public void shutdown() {
            executor.shutdownNow();
        }
    }

    /** A synchronous executor that runs tasks immediately on the calling thread. */
    static class SynchronousExecutorService extends java.util.concurrent.AbstractExecutorService {
        private boolean shutdown = false;

        @Override
        public void execute(Runnable command) {
            command.run();
        }

        @Override
        public void shutdown() {
            shutdown = true;
        }

        @Override
        public List<Runnable> shutdownNow() {
            shutdown = true;
            return Collections.emptyList();
        }

        @Override
        public boolean isShutdown() {
            return shutdown;
        }

        @Override
        public boolean isTerminated() {
            return shutdown;
        }

        @Override
        public boolean awaitTermination(long timeout, TimeUnit unit) {
            return true;
        }
    }

    @Nested
    @DisplayName("Bad Testable Design Fix: Injectable ExecutorService")
    @ExtendWith(ClearEventStudioExtension.class)
    class BadTestableDesignTest {

        private TaskExecutionService mockExecutionService;
        private UsageService mockUsageService;
        private SynchronousExecutorService synchronousExecutor;
        private TestableTaskExecutionController controller;

        @BeforeEach
        void setUp() {
            mockExecutionService = mock(TaskExecutionService.class);
            mockUsageService = mock(UsageService.class);
            synchronousExecutor = new SynchronousExecutorService();
            controller = new TestableTaskExecutionController(
                    mockExecutionService, mockUsageService, synchronousExecutor);
        }

        @Test
        @DisplayName("Synchronous executor allows deterministic testing of task execution")
        void synchronousExecution() {
            AbstractParameters params = mock(AbstractParameters.class);
            TaskExecutionRequest request = new TaskExecutionRequest("merge", params);

            controller.request(request);

            // With synchronous executor, execution happens immediately
            verify(mockExecutionService).execute(params);
            verify(mockUsageService).incrementUsageFor("merge");
            assertEquals("merge", controller.getCurrentModule());
        }

        @Test
        @DisplayName("Multiple tasks execute in order with synchronous executor")
        void orderedExecution() {
            AbstractParameters params1 = mock(AbstractParameters.class);
            AbstractParameters params2 = mock(AbstractParameters.class);

            controller.request(new TaskExecutionRequest("merge", params1));
            controller.request(new TaskExecutionRequest("split", params2));

            // Both are verified because synchronous executor runs them immediately
            verify(mockExecutionService).execute(params1);
            verify(mockExecutionService).execute(params2);
            assertEquals("split", controller.getCurrentModule());
        }

        @Test
        @DisplayName("Shutdown delegates to injected executor")
        void shutdownDelegates() {
            controller.shutdown();

            assertTrue(synchronousExecutor.isShutdown());
        }
    }

    // ========================================================================
    // SECTION 3: MOCKING — Mockito mock of PreferencesRepository
    // ========================================================================

    @Nested
    @DisplayName("Mocking: Verify DefaultRecentWorkspacesService with Mockito")
    @ExtendWith(ClearEventStudioExtension.class)
    class MockingTest {

        private PreferencesRepository mockRepo;

        @BeforeEach
        void setUp() {
            mockRepo = mock(PreferencesRepository.class);
        }

        @Test
        @DisplayName("Constructor populates cache by calling repo.keys()")
        void constructorPopulatesCache() {
            when(mockRepo.keys()).thenReturn(new String[] { "k1" });
            when(mockRepo.getString("k1", "")).thenReturn("/path/to/workspace.json");

            var service = new org.pdfsam.service.ui.DefaultRecentWorkspacesService(mockRepo);

            verify(mockRepo).keys();
            verify(mockRepo).getString("k1", "");
            assertEquals(1, service.getRecentlyUsedWorkspaces().size());
        }

        @Test
        @DisplayName("clear() calls repo.clean()")
        void clearCallsRepoClean() {
            when(mockRepo.keys()).thenReturn(new String[] {});

            var service = new org.pdfsam.service.ui.DefaultRecentWorkspacesService(mockRepo);
            service.clear();

            verify(mockRepo).clean();
        }

        @Test
        @DisplayName("onShutdown persists cached workspaces via repo.saveString()")
        void onShutdownPersists() {
            when(mockRepo.keys()).thenReturn(new String[] {});

            var service = new org.pdfsam.service.ui.DefaultRecentWorkspacesService(mockRepo);
            service.addWorkspaceLastUsed(new java.io.File("/path/to/workspace1.json"));

            service.onShutdown(new org.pdfsam.model.lifecycle.ShutdownEvent());

            // Verify clean was called before saving
            verify(mockRepo, times(1)).clean();
            // Verify saveString was called with the workspace path
            verify(mockRepo).saveString(anyString(), eq("/path/to/workspace1.json"));
        }
    }
}
