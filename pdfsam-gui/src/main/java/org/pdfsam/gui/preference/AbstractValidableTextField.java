/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.preference;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.support.validation.Validator;

import static org.pdfsam.support.RequireUtils.require;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Text field providing visual feedback for invalid input. Input is validated on Enter key pressed or on Focus lost.
 * 
 * @author Andrea Vacondio
 * 
 */
abstract class AbstractValidableTextField extends JTextField {

    private static final Color INVALID_COLOR = new Color(255, 192, 192);
    private Validator<String> validator;

    AbstractValidableTextField(Validator<String> validator) {
        require(validator != null, "Validator cannot be null");
        this.validator = validator;
        ValidateActionListener listener = new ValidateActionListener();
        addActionListener(listener);
        addFocusListener(listener);
        getDocument().addDocumentListener(new VisualValidationDocumentListener());
    }

    /**
     * called when focus is lost or enter key is pressed, after the input is validated and proved valid.
     */
    abstract void onValidInput();

    private void doVisuallyInvalid() {
        setBackground(INVALID_COLOR);
    }

    private void doVisuallyValid() {
        setBackground(Color.WHITE);
        setToolTipText(StringUtils.EMPTY);
    }

    /**
     * Steps to be executed when the input is valid and focus is lost or entry key is pressed
     * 
     * @author Andrea Vacondio
     * 
     */
    private class ValidateActionListener implements ActionListener, FocusListener {

        public void actionPerformed(ActionEvent e) {
            doValidate();
        }

        @Override
        public void focusGained(FocusEvent e) {
            // do nothing

        }

        @Override
        public void focusLost(FocusEvent e) {
            doValidate();
        }

        private void doValidate() {
            String input = getText();
            if (isNotBlank(input) && validator.isValid(input)) {
                onValidInput();
            }
        }

    }

    /**
     * Handles the visual aspect of the validation when the document changes.
     * 
     * @author Andrea Vacondio
     * 
     */
    class VisualValidationDocumentListener implements DocumentListener {

        public void insertUpdate(DocumentEvent e) {
            validationVisualElements(e);
        }

        public void removeUpdate(DocumentEvent e) {
            validationVisualElements(e);
        }

        public void changedUpdate(DocumentEvent e) {
            validationVisualElements(e);
        }

        private void validationVisualElements(DocumentEvent e) {
            String input = getText();
            if (isNotBlank(input) && !validator.isValid(input)) {
                doVisuallyInvalid();
            } else {
                doVisuallyValid();
            }
        }
    }

}
