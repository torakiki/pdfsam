package org.pdfsam.ui.components.support;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@DisplayName("Zian's FSM Test: FXValidationSupport.ValidationState")
@SuppressWarnings("unchecked")
public class ZianValidationStateFSMTest {

    private FXValidationSupport<String> validator;

    @BeforeEach
    void setUp() {
        validator = new FXValidationSupport<>();
    }

    // ==================== STATE COVERAGE TESTS ====================

    @Nested
    @DisplayName("State Coverage Tests")
    class StateCoverageTests {

        @Test
        @DisplayName("S1: NOT_VALIDATED state - initial state")
        void testNotValidatedState() {
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("S2: VALID state - after successful validation")
        void testValidState() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("valid input");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("S3: INVALID state - after failed validation")
        void testInvalidState() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("   "); // blank input
            assertEquals(ValidationState.INVALID,
                    validator.validationStateProperty().get());
        }
    }

    // ==================== VALID TRANSITION TESTS ====================

    @Nested
    @DisplayName("Valid Transition Tests")
    class ValidTransitionTests {

        @Test
        @DisplayName("T1: NOT_VALIDATED -> VALID")
        void testNotValidatedToValid() {
            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);
            validator.setValidator(Validators.nonBlank());

            validator.validate("valid input");

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.NOT_VALIDATED), eq(ValidationState.VALID));
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T2: NOT_VALIDATED -> INVALID")
        void testNotValidatedToInvalid() {
            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);
            validator.setValidator(Validators.nonBlank());

            validator.validate("");

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.NOT_VALIDATED), eq(ValidationState.INVALID));
            assertEquals(ValidationState.INVALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T3: VALID -> INVALID (re-validate with invalid input)")
        void testValidToInvalid() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("valid");
            assertEquals(ValidationState.VALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.validate("");

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.VALID), eq(ValidationState.INVALID));
            assertEquals(ValidationState.INVALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T4: INVALID -> VALID (re-validate with valid input)")
        void testInvalidToValid() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("");
            assertEquals(ValidationState.INVALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.validate("now valid");

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.INVALID), eq(ValidationState.VALID));
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T5: VALID -> NOT_VALIDATED (via setValidator)")
        void testValidToNotValidatedViaSetValidator() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("valid");
            assertEquals(ValidationState.VALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.setValidator(Validators.positiveInteger());

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.VALID), eq(ValidationState.NOT_VALIDATED));
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T6: INVALID -> NOT_VALIDATED (via makeNotValidated)")
        void testInvalidToNotValidatedViaMake() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("");
            assertEquals(ValidationState.INVALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.makeNotValidated();

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.INVALID), eq(ValidationState.NOT_VALIDATED));
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T7: VALID -> NOT_VALIDATED (via makeNotValidated)")
        void testValidToNotValidatedViaMake() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("valid");
            assertEquals(ValidationState.VALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.makeNotValidated();

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.VALID), eq(ValidationState.NOT_VALIDATED));
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("T8: INVALID -> NOT_VALIDATED (via setValidator)")
        void testInvalidToNotValidatedViaSetValidator() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("");
            assertEquals(ValidationState.INVALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.setValidator(Validators.positiveInteger());

            verify(listener).changed(any(ObservableValue.class),
                    eq(ValidationState.INVALID), eq(ValidationState.NOT_VALIDATED));
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());
        }
    }

    // ==================== SELF-LOOP TESTS ====================

    @Nested
    @DisplayName("Self-Loop Transition Tests")
    class SelfLoopTests {

        @Test
        @DisplayName("L1: VALID -> VALID (re-validate with valid input)")
        void testValidToValid() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("valid1");
            assertEquals(ValidationState.VALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.validate("valid2");

            // No state change should occur for self-loop
            verify(listener, never()).changed(any(), any(), any());
            assertEquals(ValidationState.VALID, validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("L2: INVALID -> INVALID (re-validate with invalid input)")
        void testInvalidToInvalid() {
            validator.setValidator(Validators.nonBlank());
            validator.validate("");
            assertEquals(ValidationState.INVALID, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.validate("   ");

            // No state change should occur for self-loop
            verify(listener, never()).changed(any(), any(), any());
            assertEquals(ValidationState.INVALID, validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("L3: NOT_VALIDATED -> NOT_VALIDATED (via makeNotValidated)")
        void testNotValidatedToNotValidated() {
            assertEquals(ValidationState.NOT_VALIDATED, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.makeNotValidated();

            verify(listener, never()).changed(any(), any(), any());
            assertEquals(ValidationState.NOT_VALIDATED, validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("L4: NOT_VALIDATED -> NOT_VALIDATED (via setValidator)")
        void testNotValidatedToNotValidatedViaSetValidator() {
            assertEquals(ValidationState.NOT_VALIDATED, validator.validationStateProperty().get());

            ChangeListener<ValidationState> listener = mock(ChangeListener.class);
            validator.validationStateProperty().addListener(listener);

            validator.setValidator(Validators.positiveInteger());

            verify(listener, never()).changed(any(), any(), any());
            assertEquals(ValidationState.NOT_VALIDATED, validator.validationStateProperty().get());
        }
    }

    // ==================== COMPLETE WORKFLOW TESTS ====================

    @Nested
    @DisplayName("Complete Workflow Tests")
    class WorkflowTests {

        @Test
        @DisplayName("W1: Full validation cycle: NOT_VALIDATED -> VALID -> INVALID -> VALID")
        void testFullValidationCycle() {
            validator.setValidator(Validators.nonBlank());

            // Initial state
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());

            // First validation - valid
            validator.validate("valid");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());

            // Re-validate - invalid
            validator.validate("");
            assertEquals(ValidationState.INVALID,
                    validator.validationStateProperty().get());

            // Re-validate - valid again
            validator.validate("valid again");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("W2: Reset cycle: VALID -> NOT_VALIDATED -> INVALID")
        void testResetCycle() {
            validator.setValidator(Validators.nonBlank());

            // Get to VALID state
            validator.validate("valid");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());

            // Reset to NOT_VALIDATED
            validator.makeNotValidated();
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());

            // Validate with invalid input
            validator.validate("");
            assertEquals(ValidationState.INVALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("W3: Validator change resets state")
        void testValidatorChangeResetsState() {
            // Start with nonBlank validator
            validator.setValidator(Validators.nonBlank());
            validator.validate("valid");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());

            // Change validator - should reset to NOT_VALIDATED
            validator.setValidator(Validators.positiveInteger());
            assertEquals(ValidationState.NOT_VALIDATED,
                    validator.validationStateProperty().get());

            // Validate with new validator
            validator.validate("42");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("W4: Default validator (always valid)")
        void testDefaultAlwaysValidValidator() {
            // Without setting a validator, default is always valid
            validator.validate("anything");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());

            validator.validate("");
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());

            validator.validate(null);
            assertEquals(ValidationState.VALID,
                    validator.validationStateProperty().get());
        }
    }

    // ==================== EDGE CASE TESTS ====================

    @Nested
    @DisplayName("Edge Case Tests")
    class EdgeCaseTests {

        @Test
        @DisplayName("E1: Null validator throws exception")
        void testNullValidatorThrows() {
            assertThrows(IllegalArgumentException.class, () -> validator.setValidator(null));
        }

        @Test
        @DisplayName("E2: Validate with null input")
        void testValidateWithNullInput() {
            validator.setValidator(Validators.nonBlank());

            validator.validate(null);

            assertEquals(ValidationState.INVALID, validator.validationStateProperty().get());
        }

        @Test
        @DisplayName("E3: Setting the same validator instance still resets state")
        void testSetSameValidatorResetsState() {
            org.pdfsam.core.support.validation.Validator<String> sameValidator = Validators.nonBlank();
            validator.setValidator(sameValidator);
            validator.validate("valid");
            assertEquals(ValidationState.VALID, validator.validationStateProperty().get());

            validator.setValidator(sameValidator);

            assertEquals(ValidationState.NOT_VALIDATED, validator.validationStateProperty().get());
        }
    }
}


