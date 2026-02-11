/*
 * This file is part of the PDF Split And Merge source code
 * Created for SWE 261P Software Testing and Analysis
 * Team Members: Eric, Karry, Kingson
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 */
package org.pdfsam.model.pdf;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.pdfsam.model.pdf.PdfDescriptorLoadingStatus.*;

/**
 * FSM-based tests for PdfDescriptorLoadingStatus.
 * 
 * This test class implements systematic FSM coverage including:
 * - State coverage: All states exist with correct properties
 * - Transition coverage: All valid transitions work correctly
 * - Invalid transition coverage: Invalid transitions are rejected
 * - Terminal state coverage: Terminal states are correctly identified
 * - Path coverage: Complete paths through the FSM
 * 
 * @author Eric, Karry, Kingson
 * @see PdfDescriptorLoadingStatus
 */
@DisplayName("PDF Loading Status FSM Tests")
public class PdfLoadingStatusFSMTest {

    // ========================================================================
    // State Coverage Tests
    // ========================================================================
    
    @Nested
    @DisplayName("State Coverage: All States Exist")
    class StateCoverageTests {

        @Test
        @DisplayName("All 7 states are defined")
        void allStatesExist() {
            PdfDescriptorLoadingStatus[] states = PdfDescriptorLoadingStatus.values();
            assertThat(states).hasSize(7);
            assertThat(states).contains(
                INITIAL, REQUESTED, LOADING, LOADED, 
                LOADED_WITH_USER_PWD_DECRYPTION, ENCRYPTED, WITH_ERRORS
            );
        }

        @Test
        @DisplayName("INITIAL state has no icon")
        void initialStateProperties() {
            assertThat(INITIAL.getIcon()).isNull();
            assertThat(INITIAL.getDescription()).isEmpty();
            assertThat(INITIAL.getStyle()).isEmpty();
        }

        @Test
        @DisplayName("REQUESTED state has clock icon")
        void requestedStateProperties() {
            assertThat(REQUESTED.getIcon()).isNotNull();
            assertThat(REQUESTED.getDescription()).isEmpty();
        }

        @Test
        @DisplayName("LOADING state has arrow icon")
        void loadingStateProperties() {
            assertThat(LOADING.getIcon()).isNotNull();
            assertThat(LOADING.getDescription()).isEmpty();
        }

        @Test
        @DisplayName("LOADED state has no icon (success)")
        void loadedStateProperties() {
            assertThat(LOADED.getIcon()).isNull();
            assertThat(LOADED.getDescription()).isEmpty();
        }

        @Test
        @DisplayName("LOADED_WITH_USER_PWD_DECRYPTION has unlock icon")
        void loadedWithPwdStateProperties() {
            assertThat(LOADED_WITH_USER_PWD_DECRYPTION.getIcon()).isNotNull();
            assertThat(LOADED_WITH_USER_PWD_DECRYPTION.getDescription()).isNotEmpty();
        }

        @Test
        @DisplayName("ENCRYPTED state has lock icon and warning style")
        void encryptedStateProperties() {
            assertThat(ENCRYPTED.getIcon()).isNotNull();
            assertThat(ENCRYPTED.getDescription()).isNotEmpty();
            assertThat(ENCRYPTED.getStyle()).isEqualTo("with-warnings");
        }

        @Test
        @DisplayName("WITH_ERRORS state has error icon and error style")
        void withErrorsStateProperties() {
            assertThat(WITH_ERRORS.getIcon()).isNotNull();
            assertThat(WITH_ERRORS.getDescription()).isNotEmpty();
            assertThat(WITH_ERRORS.getStyle()).isEqualTo("with-errors");
        }
    }

    // ========================================================================
    // Transition Coverage Tests - Valid Transitions
    // ========================================================================

    @Nested
    @DisplayName("Transition Coverage: Valid Transitions")
    class ValidTransitionTests {

        // From INITIAL
        @Test
        @DisplayName("INITIAL → REQUESTED: Load request initiated")
        void initialToRequested() {
            assertThat(INITIAL.canMoveTo(REQUESTED)).isTrue();
            assertThat(INITIAL.moveTo(REQUESTED)).isEqualTo(REQUESTED);
        }

        @Test
        @DisplayName("INITIAL → WITH_ERRORS: Immediate error (e.g., file not found)")
        void initialToWithErrors() {
            assertThat(INITIAL.canMoveTo(WITH_ERRORS)).isTrue();
            assertThat(INITIAL.moveTo(WITH_ERRORS)).isEqualTo(WITH_ERRORS);
        }

        // From REQUESTED
        @Test
        @DisplayName("REQUESTED → LOADING: Loading begins")
        void requestedToLoading() {
            assertThat(REQUESTED.canMoveTo(LOADING)).isTrue();
            assertThat(REQUESTED.moveTo(LOADING)).isEqualTo(LOADING);
        }

        @Test
        @DisplayName("REQUESTED → WITH_ERRORS: Pre-load error")
        void requestedToWithErrors() {
            assertThat(REQUESTED.canMoveTo(WITH_ERRORS)).isTrue();
            assertThat(REQUESTED.moveTo(WITH_ERRORS)).isEqualTo(WITH_ERRORS);
        }

        // From LOADING
        @Test
        @DisplayName("LOADING → LOADED: Successful load (no encryption)")
        void loadingToLoaded() {
            assertThat(LOADING.canMoveTo(LOADED)).isTrue();
            assertThat(LOADING.moveTo(LOADED)).isEqualTo(LOADED);
        }

        @Test
        @DisplayName("LOADING → LOADED_WITH_USER_PWD_DECRYPTION: Successful load with password")
        void loadingToLoadedWithPwd() {
            assertThat(LOADING.canMoveTo(LOADED_WITH_USER_PWD_DECRYPTION)).isTrue();
            assertThat(LOADING.moveTo(LOADED_WITH_USER_PWD_DECRYPTION)).isEqualTo(LOADED_WITH_USER_PWD_DECRYPTION);
        }

        @Test
        @DisplayName("LOADING → ENCRYPTED: Document requires password")
        void loadingToEncrypted() {
            assertThat(LOADING.canMoveTo(ENCRYPTED)).isTrue();
            assertThat(LOADING.moveTo(ENCRYPTED)).isEqualTo(ENCRYPTED);
        }

        @Test
        @DisplayName("LOADING → WITH_ERRORS: Load failed")
        void loadingToWithErrors() {
            assertThat(LOADING.canMoveTo(WITH_ERRORS)).isTrue();
            assertThat(LOADING.moveTo(WITH_ERRORS)).isEqualTo(WITH_ERRORS);
        }

        // From ENCRYPTED
        @Test
        @DisplayName("ENCRYPTED → REQUESTED: Password provided, retry load")
        void encryptedToRequested() {
            assertThat(ENCRYPTED.canMoveTo(REQUESTED)).isTrue();
            assertThat(ENCRYPTED.moveTo(REQUESTED)).isEqualTo(REQUESTED);
        }

        @Test
        @DisplayName("ENCRYPTED → WITH_ERRORS: Password operation failed")
        void encryptedToWithErrors() {
            assertThat(ENCRYPTED.canMoveTo(WITH_ERRORS)).isTrue();
            assertThat(ENCRYPTED.moveTo(WITH_ERRORS)).isEqualTo(WITH_ERRORS);
        }
    }

    // ========================================================================
    // Transition Coverage Tests - Invalid Transitions
    // ========================================================================

    @Nested
    @DisplayName("Transition Coverage: Invalid Transitions")
    class InvalidTransitionTests {

        @Test
        @DisplayName("INITIAL cannot directly reach LOADING")
        void initialCannotSkipToLoading() {
            assertThat(INITIAL.canMoveTo(LOADING)).isFalse();
            assertThatThrownBy(() -> INITIAL.moveTo(LOADING))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Cannot move status from INITIAL to LOADING");
        }

        @Test
        @DisplayName("INITIAL cannot directly reach LOADED")
        void initialCannotSkipToLoaded() {
            assertThat(INITIAL.canMoveTo(LOADED)).isFalse();
            assertThatThrownBy(() -> INITIAL.moveTo(LOADED))
                .isInstanceOf(IllegalStateException.class);
        }

        @Test
        @DisplayName("REQUESTED cannot skip to LOADED")
        void requestedCannotSkipToLoaded() {
            assertThat(REQUESTED.canMoveTo(LOADED)).isFalse();
            assertThatThrownBy(() -> REQUESTED.moveTo(LOADED))
                .isInstanceOf(IllegalStateException.class);
        }

        @Test
        @DisplayName("LOADING cannot go back to INITIAL")
        void loadingCannotGoBackToInitial() {
            assertThat(LOADING.canMoveTo(INITIAL)).isFalse();
            assertThatThrownBy(() -> LOADING.moveTo(INITIAL))
                .isInstanceOf(IllegalStateException.class);
        }

        @Test
        @DisplayName("LOADED (terminal) cannot transition anywhere")
        void loadedCannotTransition() {
            for (PdfDescriptorLoadingStatus target : PdfDescriptorLoadingStatus.values()) {
                if (target != LOADED) {
                    assertThat(LOADED.canMoveTo(target)).isFalse();
                }
            }
        }

        @Test
        @DisplayName("WITH_ERRORS (terminal) cannot transition anywhere")
        void withErrorsCannotTransition() {
            for (PdfDescriptorLoadingStatus target : PdfDescriptorLoadingStatus.values()) {
                if (target != WITH_ERRORS) {
                    assertThat(WITH_ERRORS.canMoveTo(target)).isFalse();
                }
            }
        }
    }

    // ========================================================================
    // Terminal State Coverage Tests
    // ========================================================================

    @Nested
    @DisplayName("Terminal State Coverage")
    class TerminalStateTests {

        @Test
        @DisplayName("LOADED is a terminal state")
        void loadedIsFinal() {
            assertThat(LOADED.isFinal()).isTrue();
        }

        @Test
        @DisplayName("LOADED_WITH_USER_PWD_DECRYPTION is a terminal state")
        void loadedWithPwdIsFinal() {
            assertThat(LOADED_WITH_USER_PWD_DECRYPTION.isFinal()).isTrue();
        }

        @Test
        @DisplayName("WITH_ERRORS is a terminal state")
        void withErrorsIsFinal() {
            assertThat(WITH_ERRORS.isFinal()).isTrue();
        }

        @Test
        @DisplayName("INITIAL is NOT a terminal state")
        void initialIsNotFinal() {
            assertThat(INITIAL.isFinal()).isFalse();
        }

        @Test
        @DisplayName("REQUESTED is NOT a terminal state")
        void requestedIsNotFinal() {
            assertThat(REQUESTED.isFinal()).isFalse();
        }

        @Test
        @DisplayName("LOADING is NOT a terminal state")
        void loadingIsNotFinal() {
            assertThat(LOADING.isFinal()).isFalse();
        }

        @Test
        @DisplayName("ENCRYPTED is NOT a terminal state (can retry)")
        void encryptedIsNotFinal() {
            assertThat(ENCRYPTED.isFinal()).isFalse();
        }
    }

    // ========================================================================
    // Path Coverage Tests - Complete Loading Paths
    // ========================================================================

    @Nested
    @DisplayName("Path Coverage: Complete Loading Paths")
    class PathCoverageTests {

        @Test
        @DisplayName("Happy path: Unencrypted PDF")
        void happyPathUnencrypted() {
            // INITIAL → REQUESTED → LOADING → LOADED
            PdfDescriptorLoadingStatus state = INITIAL;
            
            state = state.moveTo(REQUESTED);
            assertThat(state).isEqualTo(REQUESTED);
            
            state = state.moveTo(LOADING);
            assertThat(state).isEqualTo(LOADING);
            
            state = state.moveTo(LOADED);
            assertThat(state).isEqualTo(LOADED);
            assertThat(state.isFinal()).isTrue();
        }

        @Test
        @DisplayName("Happy path: Password-protected PDF with correct password")
        void happyPathWithPassword() {
            // INITIAL → REQUESTED → LOADING → ENCRYPTED → REQUESTED → LOADING → LOADED_WITH_USER_PWD_DECRYPTION
            PdfDescriptorLoadingStatus state = INITIAL;
            
            state = state.moveTo(REQUESTED);
            state = state.moveTo(LOADING);
            state = state.moveTo(ENCRYPTED);
            assertThat(state).isEqualTo(ENCRYPTED);
            
            // User provides password, retry
            state = state.moveTo(REQUESTED);
            state = state.moveTo(LOADING);
            state = state.moveTo(LOADED_WITH_USER_PWD_DECRYPTION);
            
            assertThat(state).isEqualTo(LOADED_WITH_USER_PWD_DECRYPTION);
            assertThat(state.isFinal()).isTrue();
        }

        @Test
        @DisplayName("Error path: Immediate error from INITIAL")
        void errorPathFromInitial() {
            // INITIAL → WITH_ERRORS (e.g., file doesn't exist)
            PdfDescriptorLoadingStatus state = INITIAL;
            
            state = state.moveTo(WITH_ERRORS);
            assertThat(state).isEqualTo(WITH_ERRORS);
            assertThat(state.isFinal()).isTrue();
        }

        @Test
        @DisplayName("Error path: Error during loading")
        void errorPathDuringLoading() {
            // INITIAL → REQUESTED → LOADING → WITH_ERRORS
            PdfDescriptorLoadingStatus state = INITIAL;
            
            state = state.moveTo(REQUESTED);
            state = state.moveTo(LOADING);
            state = state.moveTo(WITH_ERRORS);
            
            assertThat(state).isEqualTo(WITH_ERRORS);
            assertThat(state.isFinal()).isTrue();
        }

        @Test
        @DisplayName("Error path: Error from encrypted state")
        void errorPathFromEncrypted() {
            // INITIAL → REQUESTED → LOADING → ENCRYPTED → WITH_ERRORS
            PdfDescriptorLoadingStatus state = INITIAL;
            
            state = state.moveTo(REQUESTED);
            state = state.moveTo(LOADING);
            state = state.moveTo(ENCRYPTED);
            state = state.moveTo(WITH_ERRORS);
            
            assertThat(state).isEqualTo(WITH_ERRORS);
            assertThat(state.isFinal()).isTrue();
        }
    }

    // ========================================================================
    // All States Parameterized Tests
    // ========================================================================

    @Nested
    @DisplayName("Parameterized State Tests")
    class ParameterizedStateTests {

        @ParameterizedTest
        @EnumSource(PdfDescriptorLoadingStatus.class)
        @DisplayName("All states have non-null description (may be empty)")
        void allStatesHaveDescription(PdfDescriptorLoadingStatus status) {
            assertThat(status.getDescription()).isNotNull();
        }

        @ParameterizedTest
        @EnumSource(PdfDescriptorLoadingStatus.class)
        @DisplayName("All states have non-null style (may be empty)")
        void allStatesHaveStyle(PdfDescriptorLoadingStatus status) {
            assertThat(status.getStyle()).isNotNull();
        }
    }
}
