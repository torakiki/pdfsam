/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ott 2016
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.UserContext;

import javafx.scene.Parent;
import javafx.scene.control.RadioButton;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class PreferenceOutputPaneTest extends GuiTest {
    private UserContext userContext = mock(UserContext.class);

    @Override
    protected Parent getRootNode() {
        PreferenceRadioButton smartRadio = new PreferenceRadioButton(BooleanUserPreference.SMART_OUTPUT, "radio", false,
                userContext);
        smartRadio.setId("smartRadio");
        PreferenceOutputPane victim = new PreferenceOutputPane(smartRadio);
        victim.setId("victim");
        return victim;
    }

    @Test
    public void manualRadioIsSelected() {
        assertTrue(((RadioButton) find("#manualRadio")).isSelected());
    }

    @Test
    public void clickManual() {
        click("#smartRadio");
        verify(userContext).setBooleanPreference(BooleanUserPreference.SMART_OUTPUT, true);
    }

}
