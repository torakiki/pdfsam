/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.dashboard.preference;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.pdfsam.context.IntUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.validation.Validators;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferenceIntTextFieldTest extends ApplicationTest {

    private UserContext userContext = mock(UserContext.class);

    @Override
    public void start(Stage stage) {
        PreferenceIntTextField victim = new PreferenceIntTextField(IntUserPreference.THUMBNAILS_SIZE, userContext,
                Validators.positiveInteger());
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validValue() {
        clickOn("#victim").write("12").push(KeyCode.ENTER);
        verify(userContext).setIntegerPreference(IntUserPreference.THUMBNAILS_SIZE, 12);
    }

    @Test
    public void invalidValue() {
        clickOn("#victim").write("ChuckNorris").push(KeyCode.ENTER);
        verify(userContext, never()).setIntegerPreference(any(), anyInt());
    }

    @Test
    public void emptyValue() {
        clickOn("#victim").write("").push(KeyCode.ENTER);
        verify(userContext, never()).setIntegerPreference(any(), anyInt());
    }

}
