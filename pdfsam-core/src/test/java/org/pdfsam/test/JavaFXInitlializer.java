/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ago/2014
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
package org.pdfsam.test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.locks.ReentrantLock;

import javafx.embed.swing.JFXPanel;

import javax.swing.SwingUtilities;

/**
 * Perform JavaFX thread initialization
 * 
 * @author Andrea Vacondio
 *
 */
class JavaFXInitlializer {
    private ReentrantLock setupLock = new ReentrantLock();
    private boolean setUp = false;

    void init() {
        if (!setUp) {
            try {
                setupLock.lock();
                if (!setUp) {
                    setupJavaFX();
                    setUp = true;
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                setupLock.unlock();
            }
        }
    }

    private void setupJavaFX() throws InterruptedException {
        System.out.println("Initializing JavaFX thread");
        final CountDownLatch latch = new CountDownLatch(1);
        SwingUtilities.invokeLater(() -> {
            new JFXPanel();
            latch.countDown();
        });
        latch.await();
        System.out.println("JavaFX initialized");
    }
}
