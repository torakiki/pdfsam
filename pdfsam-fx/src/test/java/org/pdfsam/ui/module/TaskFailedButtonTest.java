/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
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
package org.pdfsam.ui.module;

import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.ShowStageRequest;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class TaskFailedButtonTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule("LogStage");

    @Override
    protected Parent getRootNode() {
        TaskFailedButton button = new TaskFailedButton();
        button.setMaxHeight(30);
        return button;
    }

    @Test
    public void failButtonVisibleOnFailure() {
        HitTestListener<ShowStageRequest> hit = new HitTestListener<>();
        eventStudio().add(ShowStageRequest.class, hit, "LogStage");
        click(".footer-failed-button");
        assertTrue(hit.isHit());
    }
}
