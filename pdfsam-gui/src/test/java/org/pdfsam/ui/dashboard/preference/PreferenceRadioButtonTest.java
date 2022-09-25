/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
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

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.pdfsam.core.context.BooleanUserPreference;
import org.pdfsam.core.context.UserContext;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferenceRadioButtonTest extends ApplicationTest {

    private UserContext userContext = mock(UserContext.class);

    @Override
    public void start(Stage stage) {
        Scene scene = new Scene(
                new HBox(new PreferenceRadioButton(BooleanUserPreference.SMART_OUTPUT, "select", false, userContext)));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void preferenceSetOnClick() {
        clickOn("select");
        verify(userContext).setBooleanPreference(eq(BooleanUserPreference.SMART_OUTPUT), eq(Boolean.TRUE));
    }
}
