package org.pdfsam.ui.components.tool;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.ui.components.tool.ZhenyuFooterFSMTest.TaskExecutionFSM;
import org.pdfsam.ui.components.tool.ZhenyuFooterFSMTest.TaskExecutionState;
import org.sejda.model.exception.TaskException;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.AbstractParameters;
import org.sejda.model.task.NotifiableTaskMetadata;
import java.math.BigDecimal;
import java.util.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * FSM-based tests for Footer Task Execution UI.
 * 
 * States (5):
 *   IDLE      → Initial state, RunButton enabled
 *   REQUESTED → Task requested, button disabled
 *   RUNNING   → Task executing, progress updating
 *   COMPLETED → Task finished successfully (final)
 *   FAILED    → Task failed with error (final)
 * 
 * Transitions (7):
 *   T1: IDLE      → REQUESTED (TaskExecutionRequest)
 *   T2: REQUESTED → RUNNING   (PercentageOfWorkDoneChangedEvent)
 *   T3: RUNNING   → RUNNING   (progress update self-loop)
 *   T4: RUNNING   → COMPLETED (TaskExecutionCompletedEvent)
 *   T5: RUNNING   → FAILED    (TaskExecutionFailedEvent)
 *   T6: COMPLETED → IDLE      (ready for next task)
 *   T7: FAILED    → IDLE      (ready for retry)
 */

/**
 * @author Zhenyu Song
 * @see TaskExecutionState
 */

@DisplayName("Zhenyu's Footer FSM Test")
@ExtendWith({ JavaFxThreadExtension.class, ClearEventStudioExtension.class })
public class ZhenyuFooterFSMTest {

    public enum TaskExecutionState {
        IDLE(false),
        REQUESTED(false),
        RUNNING(false),
        COMPLETED(true),
        FAILED(true);

        private final boolean isFinal;
        private Set<TaskExecutionState> validDestinations;

        TaskExecutionState(boolean isFinal) {
            this.isFinal = isFinal;
        }

        static {
            IDLE.validDestinations = Set.of(REQUESTED);
            REQUESTED.validDestinations = Set.of(RUNNING, FAILED);
            RUNNING.validDestinations = Set.of(RUNNING, COMPLETED, FAILED);
            COMPLETED.validDestinations = Set.of(IDLE);
            FAILED.validDestinations = Set.of(IDLE);
        }

        public boolean canMoveTo(TaskExecutionState dest) {
            return validDestinations.contains(dest);
        }

        public boolean isFinal() {
            return isFinal;
        }

        public Set<TaskExecutionState> getValidDestinations() {
            return validDestinations;
        }
    }

    public static class TaskExecutionFSM {
        private final ObjectProperty<TaskExecutionState> state = new SimpleObjectProperty<>(TaskExecutionState.IDLE);

        public TaskExecutionState getState() {
            return state.get();
        }

        public ObjectProperty<TaskExecutionState> stateProperty() {
            return state;
        }

        public void moveTo(TaskExecutionState newState) {
            if (!state.get().canMoveTo(newState)) {
                throw new IllegalStateException(state.get() + " -> " + newState + " is invalid");
            }
            state.set(newState);
        }

        public void reset() {
            state.set(TaskExecutionState.IDLE);
        }
    }

    private RunButton runButton;
    private TaskExecutionFSM fsm;
    private NotifiableTaskMetadata mockMetadata;
    private AbstractParameters mockParams;

    @BeforeEach
    void setUp() {
        runButton = new RunButton();
        fsm = new TaskExecutionFSM();
        mockMetadata = mock(NotifiableTaskMetadata.class);
        mockParams = mock(AbstractParameters.class);
    }

    private TaskExecutionRequest request(String toolId) {
        return new TaskExecutionRequest(toolId, mockParams);
    }

    // ==================== STATE COVERAGE ====================

    @Nested
    @DisplayName("State Coverage")
    class StateCoverage {

        @Test
        @DisplayName("IDLE: initial, button enabled")
        void idle() {
            assertEquals(TaskExecutionState.IDLE, fsm.getState());
            assertFalse(runButton.isDisabled());
            assertFalse(TaskExecutionState.IDLE.isFinal());
        }

        @Test
        @DisplayName("REQUESTED: button disabled")
        void requested() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            eventStudio().broadcast(request("test"));

            assertEquals(TaskExecutionState.REQUESTED, fsm.getState());
            assertTrue(runButton.isDisabled());
            assertFalse(TaskExecutionState.REQUESTED.isFinal());
        }

        @Test
        @DisplayName("RUNNING: task in progress")
        void running() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);

            assertEquals(TaskExecutionState.RUNNING, fsm.getState());
            assertFalse(TaskExecutionState.RUNNING.isFinal());
        }

        @Test
        @DisplayName("COMPLETED: final, button re-enabled")
        void completed() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.COMPLETED);
            eventStudio().broadcast(request("test"));
            eventStudio().broadcast(new TaskExecutionCompletedEvent(1000L, mockMetadata));

            assertEquals(TaskExecutionState.COMPLETED, fsm.getState());
            assertTrue(TaskExecutionState.COMPLETED.isFinal());
            assertFalse(runButton.isDisabled());
        }

        @Test
        @DisplayName("FAILED: final, button re-enabled")
        void failed() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.FAILED);
            eventStudio().broadcast(request("test"));
            eventStudio().broadcast(new TaskExecutionFailedEvent(new TaskException("Error"), mockMetadata));

            assertEquals(TaskExecutionState.FAILED, fsm.getState());
            assertTrue(TaskExecutionState.FAILED.isFinal());
            assertFalse(runButton.isDisabled());
        }
    }

    // ==================== TRANSITION COVERAGE ====================

    @Nested
    @DisplayName("Transition Coverage")
    class TransitionCoverage {

        @Test
        @DisplayName("IDLE → REQUESTED")
        void idleToRequested() {
            assertTrue(TaskExecutionState.IDLE.canMoveTo(TaskExecutionState.REQUESTED));
            fsm.moveTo(TaskExecutionState.REQUESTED);
            assertEquals(TaskExecutionState.REQUESTED, fsm.getState());
        }

        @Test
        @DisplayName("REQUESTED → RUNNING")
        void requestedToRunning() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            assertTrue(TaskExecutionState.REQUESTED.canMoveTo(TaskExecutionState.RUNNING));
            fsm.moveTo(TaskExecutionState.RUNNING);
            assertEquals(TaskExecutionState.RUNNING, fsm.getState());
        }

        @Test
        @DisplayName("RUNNING → RUNNING (self-loop)")
        void runningToRunning() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);

            // Multiple progress updates - stays in RUNNING
            for (int pct : new int[] { 25, 50, 75, 100 }) {
                assertTrue(fsm.getState().canMoveTo(TaskExecutionState.RUNNING));
                fsm.moveTo(TaskExecutionState.RUNNING);
                assertEquals(TaskExecutionState.RUNNING, fsm.getState());

                var event = new PercentageOfWorkDoneChangedEvent(new BigDecimal(pct), mockMetadata);
                assertEquals(pct, event.getPercentage().intValue());
            }
        }

        @Test
        @DisplayName("RUNNING → COMPLETED")
        void runningToCompleted() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            assertTrue(TaskExecutionState.RUNNING.canMoveTo(TaskExecutionState.COMPLETED));
            fsm.moveTo(TaskExecutionState.COMPLETED);
            assertEquals(TaskExecutionState.COMPLETED, fsm.getState());
        }

        @Test
        @DisplayName("RUNNING → FAILED")
        void runningToFailed() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            assertTrue(TaskExecutionState.RUNNING.canMoveTo(TaskExecutionState.FAILED));
            fsm.moveTo(TaskExecutionState.FAILED);
            assertEquals(TaskExecutionState.FAILED, fsm.getState());
        }

        @Test
        @DisplayName("COMPLETED → IDLE")
        void completedToIdle() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.COMPLETED);
            assertTrue(TaskExecutionState.COMPLETED.canMoveTo(TaskExecutionState.IDLE));
            fsm.moveTo(TaskExecutionState.IDLE);
            assertEquals(TaskExecutionState.IDLE, fsm.getState());
        }

        @Test
        @DisplayName("FAILED → IDLE")
        void failedToIdle() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.FAILED);
            assertTrue(TaskExecutionState.FAILED.canMoveTo(TaskExecutionState.IDLE));
            fsm.moveTo(TaskExecutionState.IDLE);
            assertEquals(TaskExecutionState.IDLE, fsm.getState());
        }
    }

    // ==================== INVALID TRANSITIONS ====================

    @Nested
    @DisplayName("Invalid Transitions")
    class InvalidTransitions {

        @Test
        @DisplayName("IDLE → RUNNING (must go through REQUESTED)")
        void idleToRunningInvalid() {
            assertFalse(TaskExecutionState.IDLE.canMoveTo(TaskExecutionState.RUNNING));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.RUNNING));
        }

        @Test
        @DisplayName("IDLE → COMPLETED (cannot skip states)")
        void idleToCompletedInvalid() {
            assertFalse(TaskExecutionState.IDLE.canMoveTo(TaskExecutionState.COMPLETED));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.COMPLETED));
        }

        @Test
        @DisplayName("IDLE → FAILED (cannot skip states)")
        void idleToFailedInvalid() {
            assertFalse(TaskExecutionState.IDLE.canMoveTo(TaskExecutionState.FAILED));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.FAILED));
        }

        @Test
        @DisplayName("REQUESTED → COMPLETED (must go through RUNNING)")
        void requestedToCompletedInvalid() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            assertFalse(TaskExecutionState.REQUESTED.canMoveTo(TaskExecutionState.COMPLETED));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.COMPLETED));
        }

        @Test
        @DisplayName("COMPLETED → RUNNING (must restart from IDLE)")
        void completedToRunningInvalid() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.COMPLETED);
            assertFalse(TaskExecutionState.COMPLETED.canMoveTo(TaskExecutionState.RUNNING));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.RUNNING));
        }

        @Test
        @DisplayName("FAILED → RUNNING (must restart from IDLE)")
        void failedToRunningInvalid() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.FAILED);
            assertFalse(TaskExecutionState.FAILED.canMoveTo(TaskExecutionState.RUNNING));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.RUNNING));
        }

        @Test
        @DisplayName("RUNNING → IDLE (must complete or fail first)")
        void runningToIdleInvalid() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            assertFalse(TaskExecutionState.RUNNING.canMoveTo(TaskExecutionState.IDLE));
            assertThrows(IllegalStateException.class, () -> fsm.moveTo(TaskExecutionState.IDLE));
        }
    }

    // ==================== COMPLETE PATHS ====================

    @Nested
    @DisplayName("Complete Paths")
    class CompletePaths {

        @Test
        @DisplayName("Happy Path: IDLE → REQUESTED → RUNNING → COMPLETED → IDLE")
        void happyPath() {
            assertEquals(TaskExecutionState.IDLE, fsm.getState());

            fsm.moveTo(TaskExecutionState.REQUESTED);
            eventStudio().broadcast(request("merge"));
            assertTrue(runButton.isDisabled());

            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.COMPLETED);
            eventStudio().broadcast(new TaskExecutionCompletedEvent(2000L, mockMetadata));
            assertFalse(runButton.isDisabled());
            assertTrue(TaskExecutionState.COMPLETED.isFinal());

            fsm.moveTo(TaskExecutionState.IDLE);
            assertEquals(TaskExecutionState.IDLE, fsm.getState());
        }

        @Test
        @DisplayName("Error Path: IDLE → REQUESTED → RUNNING → FAILED → IDLE")
        void errorPath() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.FAILED);
            eventStudio().broadcast(request("split"));
            eventStudio().broadcast(new TaskExecutionFailedEvent(new TaskException("Parse error"), mockMetadata));

            assertFalse(runButton.isDisabled());
            assertTrue(TaskExecutionState.FAILED.isFinal());

            fsm.moveTo(TaskExecutionState.IDLE);
            assertEquals(TaskExecutionState.IDLE, fsm.getState());
        }

        @Test
        @DisplayName("Early Error Path: REQUESTED → FAILED")
        void earlyErrorPath() {
            fsm.moveTo(TaskExecutionState.REQUESTED);
            assertTrue(fsm.getState().canMoveTo(TaskExecutionState.FAILED));
            fsm.moveTo(TaskExecutionState.FAILED);
            assertEquals(TaskExecutionState.FAILED, fsm.getState());
        }

        @Test
        @DisplayName("Retry Path: fail then succeed")
        void retryPath() {
            // First attempt fails
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.FAILED);
            fsm.moveTo(TaskExecutionState.IDLE);

            // Retry succeeds
            fsm.moveTo(TaskExecutionState.REQUESTED);
            fsm.moveTo(TaskExecutionState.RUNNING);
            fsm.moveTo(TaskExecutionState.COMPLETED);

            assertEquals(TaskExecutionState.COMPLETED, fsm.getState());
        }
    }

    // ==================== FSM MODEL VALIDATION ====================

    @Nested
    @DisplayName("FSM Model Validation")
    class FSMModelValidation {

        @Test
        @DisplayName("Final states are COMPLETED and FAILED only")
        void finalStates() {
            assertAll(
                    () -> assertFalse(TaskExecutionState.IDLE.isFinal()),
                    () -> assertFalse(TaskExecutionState.REQUESTED.isFinal()),
                    () -> assertFalse(TaskExecutionState.RUNNING.isFinal()),
                    () -> assertTrue(TaskExecutionState.COMPLETED.isFinal()),
                    () -> assertTrue(TaskExecutionState.FAILED.isFinal()));
        }

        @Test
        @DisplayName("Transition counts per state")
        void transitionCounts() {
            assertEquals(1, TaskExecutionState.IDLE.getValidDestinations().size()); // → REQUESTED
            assertEquals(2, TaskExecutionState.REQUESTED.getValidDestinations().size()); // → RUNNING, FAILED
            assertEquals(3, TaskExecutionState.RUNNING.getValidDestinations().size()); // → RUNNING, COMPLETED, FAILED
            assertEquals(1, TaskExecutionState.COMPLETED.getValidDestinations().size()); // → IDLE
            assertEquals(1, TaskExecutionState.FAILED.getValidDestinations().size()); // → IDLE
        }
    }
}
